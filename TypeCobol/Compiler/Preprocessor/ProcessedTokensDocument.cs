using System.Collections.Generic;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// View of a source document after COPY and REPLACE processing
    /// </summary>
    public class ProcessedTokensDocument : ICompilerStepDocumentSnapshot<ITokensLine, IProcessedTokensLine>
    {
        public ProcessedTokensDocument(TokensDocument previousStepSnapshot, DocumentVersion<IProcessedTokensLine> processedTokensLinesVersion, ISearchableReadOnlyList<IProcessedTokensLine> processedTokensLines, TypeCobolOptions compilerOptions)
        {
            TextSourceInfo = previousStepSnapshot.TextSourceInfo;
            PreviousStepSnapshot = previousStepSnapshot;
            CurrentVersion = processedTokensLinesVersion;
            Lines = processedTokensLines;
            _compilerOptions = compilerOptions;
        }

        private readonly TypeCobolOptions _compilerOptions;

        /// <summary>
        /// Information on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; }

        /// <summary>
        /// Snapshot of the tokens document which was used to compute the current step
        /// </summary>
        public IDocumentSnapshot<ITokensLine> PreviousStepSnapshot { get; }

        /// <summary>
        /// Document version identifier for the current document
        /// </summary>
        public DocumentVersion<IProcessedTokensLine> CurrentVersion { get; }

        /// <summary>
        /// Lines of the source text file viewed as lists of tokens and error messages
        /// </summary>
        public ISearchableReadOnlyList<IProcessedTokensLine> Lines { get; }

        /// <summary>
        /// Iterator over the tokens contained in this document after
        /// - compiler directives processing
        /// - COPY directives text imports
        /// - REPLACE directive token replacements
        /// </summary>
        public virtual ITokensLinesIterator GetProcessedTokensIterator()
        {
            ITokensLinesIterator copyIterator = new CopyTokensLinesIterator(TextSourceInfo.Name, Lines, Token.CHANNEL_SourceTokens);
            ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(copyIterator, _compilerOptions);
            return replaceIterator;
        }

        public IEnumerable<Token> GetProcessedTokens()
        {
            var tokenSource = GetProcessedTokensIterator();
            Token token;
            do
            {
                token = tokenSource.NextToken();
                yield return token;
            }
            while (token.Type != (int) TokenType.EndOfFile);
        }

        /// <summary>
        /// Iterator over all the diagnostics registered in Lines after parsing code elements
        /// </summary>
        public IEnumerable<Diagnostic> AllDiagnostics
        {
            get
            {
                foreach (var line in Lines)
                {
                    if (line.CompilerListingControlDirective?.Diagnostics != null)
                    {
                        foreach (Diagnostic diagnostic in line.CompilerListingControlDirective.Diagnostics)
                        {
                            yield return diagnostic;
                        }
                    }

                    if (line.PreprocessorDiagnostics != null)
                    {
                        foreach (Diagnostic diagnostic in line.PreprocessorDiagnostics)
                        {

                            yield return diagnostic;
                        }
                    }
                }
            }
        }
    }
}

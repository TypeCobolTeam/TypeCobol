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
        public ProcessedTokensDocument(TokensDocument previousStepSnapshot, DocumentVersion<IProcessedTokensLine> processedTokensLinesVersion, ISearchableReadOnlyList<IProcessedTokensLine> processedTokensLines, TypeCobolOptions compilerOptions, List<MissingCopy> missingCopies)
        {
            TextSourceInfo = previousStepSnapshot.TextSourceInfo;
            PreviousStepSnapshot = previousStepSnapshot;
            CurrentVersion = processedTokensLinesVersion;
            Lines = processedTokensLines;
            CompilerOptions = compilerOptions;
            MissingCopies = missingCopies;
        }

        private TypeCobolOptions CompilerOptions;

        /// <summary>
        /// Informations on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// Snapshot of the tokens document which was used to compute the current step
        /// </summary>
        public IDocumentSnapshot<ITokensLine> PreviousStepSnapshot { get; private set; }

        /// <summary>
        /// Document version identifier for the current document
        /// </summary>
        public DocumentVersion<IProcessedTokensLine> CurrentVersion { get; private set; }

        /// <summary>
        /// Lines of the source text file viewed as lists of tokens and error messages
        /// </summary>
        public ISearchableReadOnlyList<IProcessedTokensLine> Lines { get; private set; }

        /// <summary>
        /// List of all missing copies found while processing the document
        /// </summary>
        public List<MissingCopy> MissingCopies { get; }

        /// <summary>
        /// Iterator over the tokens contained in this document after
        /// - compiler directives processing
        /// - COPY directives text imports
        /// - REPLACE directive token remplacements
        /// </summary>
        public ITokensLinesIterator ProcessedTokens
        {
            get
            {
                return GetProcessedTokensIterator(TextSourceInfo, Lines,  CompilerOptions);
            }
        }

        public IEnumerable<Token> ProcessedTokensSource
        {
            get
            {
                var tokenSource = ProcessedTokens;
                Token token = null;
                do
                {
                    token = (Token)tokenSource.NextToken();
                    yield return token;
                } while (token.Type != (int)TokenType.EndOfFile);
            }
        }

        /// <summary>
        /// Iterator over the tokens contained in the parameter "lines" after
        /// - COPY directives text imports
        /// - REPLACE directive token remplacements
        /// </summary>
        public static ITokensLinesIterator GetProcessedTokensIterator(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<IProcessedTokensLine> lines, TypeCobolOptions compilerOptions)
        {
            ITokensLinesIterator copyIterator = new CopyTokensLinesIterator(textSourceInfo.Name, lines, Token.CHANNEL_SourceTokens);
            ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(copyIterator, compilerOptions);
            return replaceIterator;
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
                    if (line.CompilerListingControlDirective != null && line.CompilerListingControlDirective.Diagnostics != null)
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

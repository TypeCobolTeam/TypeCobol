using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Local view of a ProcessedTokensDocument imported by a COPY directive in another ProcessedTokensDocument.
    /// Handles a nested replace iterator to implement the REPLACING clause on top of of an tokens line iterator on the imported document.
    /// </summary>
    public class ImportedTokensDocument
    {
        public ImportedTokensDocument(CopyDirective copyDirective, ProcessedTokensDocument importedDocumentSource, PerfStatsForImportedDocument perfStats, TypeCobolOptions compilerOptions)
        {
            CopyDirective = copyDirective;
            SourceDocument = importedDocumentSource;
            HasReplacingDirective = copyDirective.ReplaceOperations.Count > 0;
            PerfStatsForImportedDocument = perfStats;
            CompilerOptions = compilerOptions;
        }

        /// <summary>
        /// Copy directive which imported this document
        /// </summary>
        public CopyDirective CopyDirective { get; private set; }

        /// <summary>
        /// Unmodified tokens document imported by the COPY directive
        /// </summary>
        public ProcessedTokensDocument SourceDocument { get; private set; }

        /// <summary>
        /// True if a REPLACING clause was applied to the imported document
        /// </summary>
        public bool HasReplacingDirective { get; private set; }

        private TypeCobolOptions CompilerOptions;

        /// <summary>
        /// Whitespace Accumulator list.
        /// </summary>
        private List<Token> Whitespaces = new List<Token>();
        /// <summary>
        /// Whitespace accumulator method
        /// </summary>
        /// <param name="token">The token to be tested as a whitespace</param>
        /// <returns>false if the token is a whitespace, true otherwise.</returns>
        private bool WhitespaceAccumulator(Token token)
        {
            if (token.Channel == Token.CHANNEL_WhitespaceAndComments)
            {
                Whitespaces.Add(token);
                return false;
            }
            return true;
        }

        /// <summary>
        /// Iterator over the tokens contained in this imported document after
        /// - REPLACING directive processing if necessary
        /// </summary>
        public ITokensLinesIterator GetProcessedTokensIterator(Func<Token, bool> tokenFilterCallback)
        {
            Func<Token, bool> tokFilterCallBack = tokenFilterCallback != null 
                ? tokenFilterCallback 
                : (HasReplacingDirective && CopyDirective.HasGroupToken ? (t => WhitespaceAccumulator(t)) : (Func<Token, bool>)null);
            ITokensLinesIterator sourceIterator = ProcessedTokensDocument.GetProcessedTokensIterator(SourceDocument.TextSourceInfo, SourceDocument.Lines, this.CompilerOptions, tokFilterCallBack);
            if (HasReplacingDirective
#if EUROINFO_RULES
                || (this.CompilerOptions.UseEuroInformationLegacyReplacingSyntax && (this. CopyDirective.RemoveFirst01Level || CopyDirective.InsertSuffixChar))
#endif
                )
            {
                ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(sourceIterator, CopyDirective, CompilerOptions);
                if (CopyDirective.HasGroupToken)
                {
                    //Compute the Scanning State
                    TypeCobol.Compiler.Scanner.MultilineScanState state = null;
                    TypeCobol.Compiler.Parser.CodeElementsLine cel = SourceDocument.Lines.Count > 0 ? ((TypeCobol.Compiler.Parser.CodeElementsLine)SourceDocument.Lines[0]) : null;
                    state = cel?.InitialScanState;

                    //Create a Preprocessed text fragment
                    StringBuilder sb = new StringBuilder();
                    Token tok;
                    bool bFirst = true;
                    ITokensLine tokenLine = null;
                    Whitespaces.Clear();
                    while ((tok = replaceIterator.NextToken()) != Token.END_OF_FILE)
                    {
                        Whitespaces.Add(tok);
                        foreach (var t in Whitespaces)
                        {
                            if (bFirst)
                            {
                                tokenLine = t.TokensLine;
                            }
                            if ((tokenLine != t.TokensLine) || bFirst)
                            {
                                if (!bFirst)
                                    sb.Append(Environment.NewLine);
                                sb.Append(new string(' ', Math.Max(0, t.Column - 1)));
                            }
                            bFirst = false;
                            tokenLine = t.TokensLine;
                            sb.Append(t.Text);
                        }
                        Whitespaces.Clear();
                    }
                    string preprocessedFRagment = sb.ToString();

                    //Now reparse the preprocessed fragment
                    ITextDocument initialTextDocumentLines = new ReadOnlyTextDocument(SourceDocument.TextSourceInfo.Name, DocumentFormat.RDZReferenceFormat.Encoding, DocumentFormat.RDZReferenceFormat.ColumnsLayout, preprocessedFRagment);
                    TypeCobolOptions tcOptions = new TypeCobolOptions();                    
                    tcOptions.AreForCopyParsing = true;//Doing that any "::" will be treated as two tokens
                    FileCompiler fileCompiler = new FileCompiler(initialTextDocumentLines, null, null, new TypeCobolOptions(), null, false, null);
                    fileCompiler.CompilationResultsForProgram.InitialScanStateForCopy = state;
                    fileCompiler.CompilationResultsForProgram.UpdateTokensLines();

                    //Create a new token iterator over rescanned source code
                    ImmutableList<CodeElementsLine>.Builder cblTextLines = (ImmutableList<CodeElementsLine>.Builder)fileCompiler.CompilationResultsForProgram.CobolTextLines;
                    TokensLinesIterator iter = new TokensLinesIterator(SourceDocument.TextSourceInfo.Name, cblTextLines, null, Token.CHANNEL_SourceTokens);
                    return iter;
                }
                else
                {
                    return replaceIterator;
                }
            }
            else
            {
                return sourceIterator;
            }
        }

        /// <summary>
        /// Performance metrics for compilation documents retrieved in cache
        /// </summary>
        public PerfStatsForImportedDocument PerfStatsForImportedDocument { get; private set; }
    }
}

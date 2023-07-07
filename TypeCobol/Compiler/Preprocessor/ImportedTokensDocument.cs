using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

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
        /// Iterator over the tokens contained in this imported document after
        /// - REPLACING directive processing if necessary
        /// </summary>
        public ITokensLinesIterator GetProcessedTokensIterator()
        {
            ITokensLinesIterator iterator = new CopyTokensLinesIterator(SourceDocument.TextSourceInfo.Name, SourceDocument.Lines, Token.CHANNEL_SourceTokens);

            if (HasReplacingDirective
#if EUROINFO_RULES
                || (CopyDirective.RemoveFirst01Level || CopyDirective.InsertSuffixChar)
#endif
               )
            {
                iterator = new ReplacingTokensLinesIterator(iterator, CopyDirective, CompilerOptions);
            }

            return iterator;
        }

        /// <summary>
        /// Performance metrics for compilation documents retrieved in cache
        /// </summary>
        public PerfStatsForImportedDocument PerfStatsForImportedDocument { get; private set; }
    }
}

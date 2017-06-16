using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        public ImportedTokensDocument(CopyDirective copyDirective, ProcessedTokensDocument importedDocumentSource, PerfStatsForImportedDocument perfStats)
        {
            CopyDirective = copyDirective;
            SourceDocument = importedDocumentSource;
            HasReplacingDirective = copyDirective.ReplaceOperations.Count > 0;
            PerfStatsForImportedDocument = perfStats;
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

        /// <summary>
        /// Iterator over the tokens contained in this imported document after
        /// - REPLACING directive processing if necessary
        /// </summary>
        public ITokensLinesIterator GetProcessedTokensIterator()
        {
            ITokensLinesIterator sourceIterator = ProcessedTokensDocument.GetProcessedTokensIterator(SourceDocument.TextSourceInfo, SourceDocument.Lines);
            if (HasReplacingDirective
#if EUROINFO_LEGACY_REPLACING_SYNTAX
                || CopyDirective.RemoveFirst01Level || CopyDirective.InsertSuffixChar
#endif
                )
            {
                ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(sourceIterator, CopyDirective);
                return replaceIterator;
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

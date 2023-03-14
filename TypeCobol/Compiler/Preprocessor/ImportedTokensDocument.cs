using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Directives;
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
        /// Iterator over the tokens contained in this imported document after
        /// - REPLACING directive processing if necessary
        /// </summary>
        public ITokensLinesIterator GetProcessedTokensIterator()
        {

            ITokensLinesIterator copyIterator = new CopyTokensLinesIterator(SourceDocument.TextSourceInfo.Name, SourceDocument.Lines, Token.CHANNEL_SourceTokens);
            ITokensLinesIterator replacingIterator = null;

            if (HasReplacingDirective
#if EUROINFO_RULES
                || this.CompilerOptions.UseEuroInformationLegacyReplacingSyntax && (this.CopyDirective.RemoveFirst01Level || CopyDirective.InsertSuffixChar)
#endif
                )
            {
                replacingIterator = new Replacing(copyIterator, CopyDirective, CompilerOptions);
            }

            var replaceIterator = new ReplaceTokensLinesIterator( replacingIterator ?? copyIterator, CompilerOptions);
            return replaceIterator;

            
        }

        /// <summary>
        /// Performance metrics for compilation documents retrieved in cache
        /// </summary>
        public PerfStatsForImportedDocument PerfStatsForImportedDocument { get; private set; }
    }
}

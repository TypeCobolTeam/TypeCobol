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
    /// All TokensLines from the imported document which are not impacted by either :
    /// - the REPLACING clause of the COPY directive in the host document, 
    /// - or a REPLACE directive in the host document
    /// are directly referenced with no dedicated memory allocation.
    /// If at least one token of the imported document needs to be replaced in the context of the host document,
    /// a new TokensLine is created and replaced tokens specific to the host document are inserted.
    /// The imported ProcessedTokensDocument is itself never modified.
    /// </summary>
    public class ImportedTokensDocument
    {
        public ImportedTokensDocument(CopyDirective copyDirective, ProcessedTokensDocument importedDocumentSource)
        {
            CopyDirective = copyDirective;
            SourceDocument = importedDocumentSource;
            HasReplacingDirective = copyDirective.ReplaceOperations.Count > 0;
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
        public ITokensLinesIterator GetTokensIterator()
        {
            ITokensLinesIterator sourceIterator = SourceDocument.GetTokensIterator();
            if (HasReplacingDirective)
            {
                ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(sourceIterator, CopyDirective);
                return replaceIterator;
            }
            else
            {
                return sourceIterator;
            }
        }
    }
}

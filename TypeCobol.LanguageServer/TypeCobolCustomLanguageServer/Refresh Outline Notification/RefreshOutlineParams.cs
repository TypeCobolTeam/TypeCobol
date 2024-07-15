using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class RefreshOutlineParams
    {
        /// <summary>
        /// The Text Document
        /// </summary>
        public TextDocumentIdentifier textDocument { get; set; }


        /// <summary>
        /// The List of OutlineNode concerned
        /// </summary>
        public List<OutlineNode> outlineNodes;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="uri">The Document's identifier</param>
        /// <param name="rootOutlineNode">The List of concerned OutlineNode</param>
        public RefreshOutlineParams(TextDocumentIdentifier textDocument, OutlineNode rootOutlineNode)
        {
            this.textDocument = textDocument;
            this.outlineNodes = rootOutlineNode.childNodes;
        }
    }
}

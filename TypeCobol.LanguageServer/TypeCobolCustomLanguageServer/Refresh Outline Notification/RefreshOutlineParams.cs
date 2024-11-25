using TypeCobol.LanguageServer.VsCodeProtocol;

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
    }
}

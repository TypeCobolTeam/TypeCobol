using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class NodeRefreshParams
    {
        public TextDocumentIdentifier textDocument { get; set; }
    }
}

using System.Collections.Generic;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class NodeRefreshParams
    {
        public VsCodeProtocol.TextDocumentIdentifier textDocument { get; set; }
    }
}

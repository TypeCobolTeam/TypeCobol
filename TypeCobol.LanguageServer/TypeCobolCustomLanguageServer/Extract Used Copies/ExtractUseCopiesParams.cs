using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class ExtractUseCopiesParams
    {
        public TextDocumentIdentifier textDocument { get; set; }
    }
}

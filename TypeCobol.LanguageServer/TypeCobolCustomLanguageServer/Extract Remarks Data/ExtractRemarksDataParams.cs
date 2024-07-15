#if EUROINFO_RULES

using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class ExtractRemarksDataParams
    {
        public TextDocumentIdentifier textDocument;
    }
}

#endif

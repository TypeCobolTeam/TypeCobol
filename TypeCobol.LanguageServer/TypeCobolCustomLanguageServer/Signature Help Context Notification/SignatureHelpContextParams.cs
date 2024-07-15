using System.Collections.Generic;
using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class SignatureHelpContextParams
    {
        public TextDocumentIdentifier textDocument { get; set; }
        public SignatureInformation signatureInformation { get; set; }
    }
}

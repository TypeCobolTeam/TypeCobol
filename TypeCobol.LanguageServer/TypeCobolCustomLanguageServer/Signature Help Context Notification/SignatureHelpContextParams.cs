using System.Collections.Generic;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class SignatureHelpContextParams
    {
        public TextDocumentIdentifier textDocument { get; set; }
        public SignatureInformation signatureInformation { get; set; }
    }
}

using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class SignatureHelpContextNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/signatureHelpContext", typeof(SignatureHelpContextParams));
    }
}

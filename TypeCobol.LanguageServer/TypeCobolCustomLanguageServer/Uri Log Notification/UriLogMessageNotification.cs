using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The log message notification is send from the server to the client to ask
    /// the client to log a particular message.
    /// </summary>
    public class UriLogMessageNotification
    {
        public static readonly NotificationType Type = new NotificationType("window/logMessage", typeof(UriLogMessageParams));
    }
}

using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class DocumentConfigurationChangedNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/documentConfigurationChanged", typeof(DocumentConfigurationChangedParams));
    }
}

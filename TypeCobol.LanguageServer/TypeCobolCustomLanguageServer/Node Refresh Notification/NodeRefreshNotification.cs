using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class NodeRefreshNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/refreshNodes", typeof(NodeRefreshParams));
    }
}

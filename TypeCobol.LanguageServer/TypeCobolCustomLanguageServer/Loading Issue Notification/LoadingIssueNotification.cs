using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class LoadingIssueNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/loadingIssue", typeof(LoadingIssueParams));
    }
}

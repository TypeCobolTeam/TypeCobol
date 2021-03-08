using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class CfgDfaNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/CfgDfa", typeof(CfgDfaParams));
    }
}

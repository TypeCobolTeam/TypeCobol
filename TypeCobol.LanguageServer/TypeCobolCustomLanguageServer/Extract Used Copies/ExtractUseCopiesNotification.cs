using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class ExtractUseCopiesNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/extractusecopy", typeof(ExtractUseCopiesParams));
    }
}

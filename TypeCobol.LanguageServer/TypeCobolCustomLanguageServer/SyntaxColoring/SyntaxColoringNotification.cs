using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring
{
    public class SyntaxColoringNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/syntaxColoring", typeof(SyntaxColoringParams));
    }
}

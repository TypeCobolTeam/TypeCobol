using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The configuration change notification is sent from the client to the server
    /// when a client's project configuration has changed.
    /// </summary>
    class DidChangeProjectConfigurationNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/workspace/didChangeProjectConfiguration", typeof(DidChangeProjectConfigurationParams));
    }
}

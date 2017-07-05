using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The missing copies notification is sent from the server to the client or from the client to the server.
    /// When the server detect missing copies after TypeCobol parsing it will send a notification to the client
    /// the client will have to load the missing copies. In case he can't load all the copies, 
    /// he'll send a notification to the server with the list of copies that could not be loaded. 
    /// So the server will create a diagnostic notification to the client. 
    /// </summary>
    class MissingCopiesNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/missingCopies", typeof(MissingCopiesParams));
    }
}

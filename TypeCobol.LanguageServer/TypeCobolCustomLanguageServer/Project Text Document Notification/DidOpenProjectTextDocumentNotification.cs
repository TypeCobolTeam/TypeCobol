using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The project text document open notification is sent from the client to the server to signal
    /// newly opened text document that belongs to a specific project.The document's truth is now managed by the client
    /// and the server must not try to read the document's truth using the document's
    /// uri.
    /// Open in this sense means it is managed by the client. 
    /// It doesn’t necessarily mean that its content is presented in an editor. 
    /// An open notification must not be sent more than once without a corresponding close notification send before. 
    /// This means open and close notification must be balanced and the max open count for a particular textDocument is one. 
    /// Note that a server’s ability to fulfill requests is independent of whether a text document is open or closed.
    /// </summary>

    class DidOpenProjectTextDocumentNotification
    {
        public static readonly NotificationType Type = new NotificationType("typecobol/projectTextDocument/didOpen", typeof(DidOpenProjectTextDocumentParams));
    }
}

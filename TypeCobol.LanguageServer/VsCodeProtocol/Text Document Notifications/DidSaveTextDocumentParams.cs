namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The document save notification is sent from the client to the server when the document for saved in the client.
    /// </summary>
    public class DidSaveTextDocumentParams
    {
  
        /// <summary>
        /// The document that was saved.
        /// </summary>
        public TextDocumentIdentifier textDocument;

        /// <summary>
        /// Optional the content when saved. Depends on the includeText value when the save notification was requested.
        /// </summary>
        public string text;
    }
}

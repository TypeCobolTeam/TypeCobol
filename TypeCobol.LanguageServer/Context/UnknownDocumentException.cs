namespace TypeCobol.LanguageServer.Context
{
    /// <summary>
    /// Exception used to signal that a document could not be found by this server
    /// using the URI given by the client.
    /// </summary>
    public class UnknownDocumentException : Exception
    {
        public UnknownDocumentException(string uri)
            : base($"Unknown document: '{uri}'")
        {

        }
    }
}

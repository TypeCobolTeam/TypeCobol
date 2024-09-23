namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// An identifier to denote a specific version of a text document.
    /// </summary>
    public class VersionedTextDocumentIdentifier : TextDocumentIdentifier
    {
        /// <summary>
        /// The version number of this document.
        /// </summary>
        public int? version { get; set; }

        /// <summary>
        /// Creates a new versioned TextDocumentIdentifier.
        /// </summary>
        /// <param name="uri">The document's uri.</param>
        /// <param name="version">The version number of this document.</param>
        public VersionedTextDocumentIdentifier(string uri, int? version) : base(uri) { this.version = version; }
    }
}

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
        public int version { get; set; }
    }
}

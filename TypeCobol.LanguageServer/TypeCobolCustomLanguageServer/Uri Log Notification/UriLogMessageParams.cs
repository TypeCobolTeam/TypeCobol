using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The log message parameters associated to a text document uri.
    /// </summary>
    class UriLogMessageParams
    {
        /// <summary>
        /// The message type. See {@link MessageType}
        /// </summary>
        public VsCodeProtocol.MessageType type { get; set; }

        /// <summary>
        /// The actual message
        /// </summary>
        public string message { get; set; }

        /// <summary>
        /// The target text document
        /// </summary>
        public TextDocumentIdentifier textDocument { get; set; }
    }
}

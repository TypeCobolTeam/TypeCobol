using System.Collections.Generic;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters of a missing copies notification
    /// </summary>
    class MissingCopiesParams
    {
        /// <summary>
        /// The TextDocumentIdentifier for which diagnostic information is reported.
        /// </summary>
        public VsCodeProtocol.TextDocumentIdentifier textDocument { get; set; }
        /// <summary>
        /// List of missing copies
        /// </summary>
        public List<string> Copies { get; set; }
    }
}

using System.Collections.Generic;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters of a missing copies notification
    /// </summary>
    class MissingCopiesParams
    {
        /// <summary>
        /// List of missing copies
        /// </summary>
        public List<string> Copies { get; set; }
    }
}

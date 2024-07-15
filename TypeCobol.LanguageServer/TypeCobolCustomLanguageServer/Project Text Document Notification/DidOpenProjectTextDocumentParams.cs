using System.Collections.Generic;
using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters send in a open project text document notification
    /// </summary>
    public class DidOpenProjectTextDocumentParams : DidOpenTextDocumentParams
    {
        /// <summary>
        /// The uniquely identified project's key to which belongs the text document opened.
        /// </summary>
        public string ProjectKey;

        /// <summary>
        /// List of copy folders, associated to the Project whose key is given by projectKey property.
        /// </summary>
        public List<string> CopyFolders;

        //TODO LSP UPGRADE
    }
}

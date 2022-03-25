using System.Collections.Generic;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters sent in an open project text document notification
    /// </summary>
    public class DidOpenProjectTextDocumentParams : DidOpenTextDocumentParams
    {
        /// <summary>
        /// The uniquely identified project's key to which belongs the text document opened.
        /// </summary>
        public string ProjectKey;

        /// <summary>
        /// List of copy folders, associated to the Project whose key is given by ProjectKey property.
        /// </summary>
        public List<string> CopyFolders;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="textDocument"></param>
        /// <param name="text"></param>
        /// <param name="projectKey"></param>
        /// <param name="copyFolders"></param>
        public DidOpenProjectTextDocumentParams(TextDocumentItem textDocument, string text, string projectKey, List<string> copyFolders)
            : base(textDocument, text)
        {
            this.ProjectKey = projectKey;
            this.CopyFolders = copyFolders;
        }
    }
}

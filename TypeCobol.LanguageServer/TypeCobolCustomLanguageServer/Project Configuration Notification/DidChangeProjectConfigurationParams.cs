using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class DidChangeProjectConfigurationParams
    {
        /// <summary>
        /// The TextDocumentIdentifier under which this project configuration changed notification has been sent.
        /// Null if the notification is not related to a text document.
        /// </summary>
        public VsCodeProtocol.TextDocumentIdentifier textDocument { get; set; }
        /// <summary>
        /// The unique key of the associated project, can be null if no project is associated.
        /// Keys are not case sensitive.
        /// </summary>
        public string ProjectKey { get; set; }
        /// <summary>
        /// List of copy folders, associated to the Project whose key is given by ProjectKey property.
        /// </summary>
        public List<string> CopyFolders { get; set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="textDocument"></param>
        /// <param name="projectKey"></param>
        /// <param name="copyFolders"></param>
        public DidChangeProjectConfigurationParams(VsCodeProtocol.TextDocumentIdentifier textDocument, string projectKey, List<string> copyFolders)
        {
            this.textDocument = textDocument;
            this.ProjectKey = projectKey;
            this.CopyFolders = copyFolders;
        }
    }
}

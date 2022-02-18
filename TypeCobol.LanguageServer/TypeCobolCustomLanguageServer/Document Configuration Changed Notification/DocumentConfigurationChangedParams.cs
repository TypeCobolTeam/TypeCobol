using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class DocumentConfigurationChangedParams
    {
        /// <summary>
        /// The TextDocumentIdentifier for which configuration have changed.
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
    }
}

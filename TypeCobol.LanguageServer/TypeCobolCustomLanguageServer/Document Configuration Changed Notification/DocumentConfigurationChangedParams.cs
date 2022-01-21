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
        /// The TextDocumentIdentifier for which diagnostic information is reported.
        /// </summary>
        public VsCodeProtocol.TextDocumentIdentifier textDocument { get; set; }
        /// <summary>
        /// List of copy folders
        /// </summary>
        public List<string> CopyFolders { get; set; }
    }
}

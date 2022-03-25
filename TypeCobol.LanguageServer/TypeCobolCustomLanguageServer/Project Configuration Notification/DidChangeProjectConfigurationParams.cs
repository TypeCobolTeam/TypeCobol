using System.Collections.Generic;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class DidChangeProjectConfigurationParams
    {
        /// <summary>
        /// The unique key of the target project.
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
        /// <param name="projectKey"></param>
        /// <param name="copyFolders"></param>
        public DidChangeProjectConfigurationParams(string projectKey, List<string> copyFolders)
        {
            this.ProjectKey = projectKey;
            this.CopyFolders = copyFolders;
        }
    }
}

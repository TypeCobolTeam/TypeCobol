namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class DidChangeProjectConfigurationParams
    {
        /// <summary>
        /// The unique key of the target project.
        /// Keys are not case-sensitive.
        /// </summary>
        public string ProjectKey { get; set; }

        /// <summary>
        /// List of copy folders, associated to the Project whose key is given by ProjectKey property.
        /// </summary>
        public List<string> CopyFolders { get; set; }
    }
}

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Class representing the response sent for a workspace/applyEdit request.
    /// </summary>
    public class ApplyWorkspaceEditResponse
    {
        /// <summary>
        /// Gets or sets a value indicating whether edits were applied or not.
        /// </summary>
        public bool applied { get; set; }

        /// <summary>
        /// Gets or sets a string with textual description for why the edit was not applied.
        /// </summary>
        public string failureReason { get; set; }
    }
}

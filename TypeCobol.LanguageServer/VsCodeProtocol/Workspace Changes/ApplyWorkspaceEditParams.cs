namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Class representing the parameters sent from a server to a client for the workspace/applyEdit request.
    /// </summary>
    public class ApplyWorkspaceEditParams
    {
        /// <summary>
        /// Gets or sets the label associated with this edit.
        /// </summary>
        public string label { get; set; }

        /// <summary>
        /// Gets or sets the edit to be applied to the workspace.
        /// </summary>
        public WorkspaceEdit edit { get; set; }
    }
}

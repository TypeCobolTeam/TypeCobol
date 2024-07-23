namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Class representing the parameters sent from client to server for the workspace/executeCommand request.
    /// </summary>
    public class ExecuteCommandParams
    {
        /// <summary>
        /// Gets or sets the command identifier associated with the command handler.
        /// </summary>
        public string command { get; set; }

        /// <summary>
        /// Gets or sets the arguments that the command should be invoked with.
        /// </summary>
        public object[] arguments { get; set; }
    }
}

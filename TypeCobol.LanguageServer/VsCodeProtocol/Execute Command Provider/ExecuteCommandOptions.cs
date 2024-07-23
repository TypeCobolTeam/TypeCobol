namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Class representing the options for execute command support.
    /// </summary>
    public class ExecuteCommandOptions
    {
        /// <summary>
        /// Gets or sets the commands that are to be executed on the server.
        /// </summary>
        public string[] commands { get; set; }
    }
}

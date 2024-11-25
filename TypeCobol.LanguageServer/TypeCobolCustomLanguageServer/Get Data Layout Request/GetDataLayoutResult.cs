namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The result returned by the server to a GetDataLayout request
    /// </summary>
    public class GetDataLayoutResult
    {
        /// <summary>
        /// The result dedicated to CSV outputType
        /// </summary>
        public GetDataLayoutCSVResult csvResult { get; set; }
    }

    /// <summary>
    /// The result for a CSV output
    /// </summary>
    public class GetDataLayoutCSVResult
    {
        internal const string SEPARATOR = ";";

        /// <summary>
        /// The table header
        /// </summary>
        public string header { get; set; }

        /// <summary>
        /// The table rows
        /// </summary>
        public string[] rows { get; set; }

        /// <summary>
        /// The separator
        /// </summary>
        public string separator { get; set; }
    }
}

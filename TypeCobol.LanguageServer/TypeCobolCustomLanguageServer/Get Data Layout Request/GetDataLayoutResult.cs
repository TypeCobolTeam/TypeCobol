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
        public GetDataLayoutCSVResult csvResult { get; internal set; }

        public GetDataLayoutResult(string[] rows) => csvResult = new GetDataLayoutCSVResult(rows);
    }

    /// <summary>
    /// The result for a CSV output
    /// </summary>
    public class GetDataLayoutCSVResult
    {
        // For outputType = CSV
        internal const string SEPARATOR = ";";
        private const string HEADER = "LineNumber;NodeLevel;LevelNumber;VariableName;PictureTypeOrUsage;Start;End;Length";

        /// <summary>
        /// The table header
        /// </summary>
        public string header { get; internal set; }

        /// <summary>
        /// The table rows
        /// </summary>
        public string[] rows { get; internal set; }

        /// <summary>
        /// The separator
        /// </summary>
        public string separator { get; internal set; }

        public GetDataLayoutCSVResult(string[] rows) => (header, this.rows, separator) = (HEADER, rows, SEPARATOR);
    }
}

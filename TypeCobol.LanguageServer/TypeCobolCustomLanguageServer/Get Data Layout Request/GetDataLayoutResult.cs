namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The result returned by the server to a GetDataLayout request
    /// </summary>
    public class GetDataLayoutResult
    {
        /// <summary>
        /// The output type of the result (only CSV is currently supported)
        /// </summary>
        public string outputType { get; internal set; }

        /// <summary>
        /// The table header (for outputType = CSV)
        /// </summary>
        public string header { get; internal set; }

        /// <summary>
        /// The table rows (for outputType = CSV)
        /// </summary>
        public string[] rows { get; internal set; }

        /// <summary>
        /// The separator (for outputType = CSV)
        /// </summary>
        public string separator { get; internal set; }

        public GetDataLayoutResult(string[] rows) => (outputType, header, this.rows, separator) = (GetDataLayoutConstants.OUTPUT_TYPE_CSV, GetDataLayoutConstants.HEADER, rows, GetDataLayoutConstants.SEPARATOR);
    }
}

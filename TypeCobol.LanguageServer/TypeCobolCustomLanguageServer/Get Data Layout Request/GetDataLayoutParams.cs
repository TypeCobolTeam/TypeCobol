namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters related to GetDataLayout request
    /// </summary>
    class GetDataLayoutParams
    {
        // Output types
        internal const string OUTPUT_TYPE_CSV = "CSV";
        internal const string OUTPUT_TYPE_TREE = "TREE";

        /// <summary>
        /// The document and the position that request the Data Layout
        /// </summary>
        public VsCodeProtocol.TextDocumentPosition textDocumentPosition { get; set; }

        /// <summary>
        /// The output type expected by the client (CSV or TREE). If none is specified the default is CSV.
        /// </summary>
        public string outputType { get; set; }
    }
}

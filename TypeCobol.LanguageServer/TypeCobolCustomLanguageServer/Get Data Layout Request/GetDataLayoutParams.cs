namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters related to GetDataLayout request
    /// </summary>
    class GetDataLayoutParams
    {
        // Output types (add TREE in the future?)
        internal const string OUTPUT_TYPE_CSV = "CSV";

        /// <summary>
        /// The document and the position that request the Data Layout
        /// </summary>
        public VsCodeProtocol.TextDocumentPosition textDocumentPosition { get; set; }

        /// <summary>
        /// The output type expected by the client (only CSV is currently supported)
        /// </summary>
        public string outputType { get; set; }
    }
}

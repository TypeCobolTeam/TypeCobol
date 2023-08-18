namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters related to GetDataLayout request
    /// </summary>
    class GetDataLayoutParams
    {
        /// <summary>
        /// The current document in edition
        /// </summary>
        public VsCodeProtocol.TextDocumentIdentifier textDocument { get; set; }

        /// <summary>
        /// The output type expected by the client (only CSV is currently supported)
        /// </summary>
        public string outputType { get; set; }
    }
}

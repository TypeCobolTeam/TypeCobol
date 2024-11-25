namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The parameters of CFG/DFA information notification
    /// </summary>
    public class CfgDfaParams
    {
        /// <summary>
        /// Basic Block Informations.
        /// </summary>
        public class BasicBlockInfo
        {
            /// <summary>
            /// Basic block identifier.
            /// </summary>
            public int id { get; set; }
            /// <summary>         
            /// Any Sub graph id to which this block belongs to.
            /// </summary>
            public int subgraphId { get; set; }
            /// <summary>
            /// Index of the first instruction in the instruction range array
            /// </summary>
            public int fstInstIdx { get; set; }
            /// <summary>
            /// Index of the last instruction in the instruction range array
            /// </summary>
            public int lstInstIdx { get; set; }
        }

        /// <summary>
        /// The TextDocumentIdentifier for which cfg/dfa information is reported.
        /// </summary>
        public VsCodeProtocol.TextDocumentIdentifier textDocument { get; set; }
        /// <summary>
        /// The Path to the associated .dot File if any.
        /// The File if it exists is a temporary file it shall be deleted by the client.
        /// Can be null
        /// </summary>
        public string dotFilePath { get; set; }
        /// <summary>
        /// The Dot content if the dotFilePath is not provided.
        /// Can be null
        /// </summary>
        public string dotContent { get; set; }
        /// <summary>
        /// An array of instruction positions in the source document.
        /// </summary>
        public VsCodeProtocol.Position[] instructionPositions { get; set; }   
        /// <summary>
        /// An array of BasicBlock information.
        /// </summary>
        public BasicBlockInfo[] basicBlockInfos { get; set; }
    }
}

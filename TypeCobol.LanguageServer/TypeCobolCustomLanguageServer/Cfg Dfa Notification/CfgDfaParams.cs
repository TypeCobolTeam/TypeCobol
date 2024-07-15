using Microsoft.VisualStudio.LanguageServer.Protocol;

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
        public TextDocumentIdentifier textDocument { get; set; }
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
        public Position[] instructionPositions { get; set; }   
        /// <summary>
        /// An array of BasicBlock information.
        /// </summary>
        public BasicBlockInfo[] basicBlockInfos { get; set; }

        /// <summary>
        /// Full constructor
        /// </summary>
        /// <param name="textDocument">The Target document identifier</param>
        /// <param name="dotFilePath">The path of the dot file</param>
        /// <param name="instructionPositions">The array of instruction positions</param>
        /// <param name="basicBlockInfos">The Basic Block Information array</param>
        public CfgDfaParams(TextDocumentIdentifier textDocument, string dotFilePath, Position[] instructionPositions, BasicBlockInfo[] basicBlockInfos)
        {
            this.textDocument = textDocument;
            this.dotFilePath = dotFilePath;
            this.instructionPositions = instructionPositions;
            this.basicBlockInfos = basicBlockInfos;
        }

        /// <summary>
        /// Empty Constructor
        /// </summary>
        /// <param name="textDocument">The Target document identifier</param>
        public CfgDfaParams(TextDocumentIdentifier textDocument)
            : this(textDocument, null, null, null)
        {            
        }
    }
}

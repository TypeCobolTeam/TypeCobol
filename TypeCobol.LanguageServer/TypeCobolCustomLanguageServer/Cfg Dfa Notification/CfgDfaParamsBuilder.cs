using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.VsCodeProtocol;
using static TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.CfgDfaParams;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// Class that help to build a CfgDfaParams instances.
    /// </summary>
    public class CfgDfaParamsBuilder
    {
        private TextDocumentIdentifier textDocumentId;
        private string dotFilePath;
        private List<Position> instructionPositions;
        private List<BasicBlockInfo> basicBlockInfos;
        /// <summary>
        /// Resulting CfgDfaParams instance.
        /// </summary>
        public CfgDfaParams GetParams() {
            return new CfgDfaParams()
            {
                textDocument = textDocumentId,
                dotFilePath = dotFilePath,
                instructionPositions = instructionPositions.ToArray(),
                basicBlockInfos = basicBlockInfos.ToArray()
            };
        }

        public CfgDfaParamsBuilder(TextDocumentIdentifier textDocId, string dotFilePath) 
        {
            this.textDocumentId = textDocId;
            this.dotFilePath = dotFilePath;
            instructionPositions = new List<Position>();
            basicBlockInfos = new List<BasicBlockInfo>();
        }

        /// <summary>
        /// Add a new Basic block 
        /// </summary>
        /// <typeparam name="D"></typeparam>
        /// <param name="block">The Basic block to be added</param>
        /// <param name="subgraphIndex">Index of the Subgraph to which belongs the block</param>
        public void AddBlock<D>(BasicBlock<Node,D> block, int subgraphIndex)
        {
            int firstInstrIndex = block.Instructions.Count > 0 ? instructionPositions.Count : -1;
            foreach (var i in block.Instructions)
            {
                instructionPositions.Add(new Position(i.CodeElement.Line - 1, i.CodeElement.Column - 1));
            }
            int lastInstrIndex = block.Instructions.Count > 0 ? instructionPositions.Count : -1;
            BasicBlockInfo blockInfo = new BasicBlockInfo()
            {
                id = block.Index,
                subgraphId = subgraphIndex,
                fstInstIdx = firstInstrIndex,
                lstInstIdx = lastInstrIndex
            };
            basicBlockInfos.Add(blockInfo);
        }
    }
}

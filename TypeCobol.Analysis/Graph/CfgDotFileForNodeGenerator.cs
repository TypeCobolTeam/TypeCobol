using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// Graphviz Dot file generator for a Control Flow Graph
    /// </summary>
    /// <typeparam name="D"></typeparam>
    public class CfgDotFileForNodeGenerator<D> : CfgDotFileGenerator<Node, D>
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg"></param>
        public CfgDotFileForNodeGenerator()
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">The underlying Control Flow Graph</param>
        public CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg) : base (cfg)
        {
        }

        /// <summary>
        /// Set whether full instruction must be generated are not.
        /// If not only the instruction name will be generated.
        /// </summary>
        public bool FullInstruction
        {
            get;
            set;
        }

        /// <summary>
        /// Get the string representing an instruction.
        /// </summary>
        /// <param name="instruction">The instruction to get the string representation.</param>
        protected override string InstructionToString(Node instruction)
        {
            return (instruction == null || instruction.CodeElement == null) ? "<null>" :
                FullInstruction ? Escape(instruction.CodeElement.SourceText) :
                System.Enum.GetName(typeof(CodeElementType), instruction.CodeElement.Type);
        }

        /// <summary>
        /// Get the dot format name of a block.
        /// </summary>
        /// <param name="block">The block to get the dot format name.</param>
        /// <returns>The block's name</returns>
        protected override string BlockName(BasicBlock<Node, D> block)
        {
            string name = base.BlockName(block);
            ControlFlowGraphBuilder<D>.BasicBlockForNode nodeBlock = (ControlFlowGraphBuilder<D>.BasicBlockForNode)block;
            if (!block.HasFlag(BasicBlock<Node, D>.Flags.Start) && nodeBlock.Tag != null)
            {
                string tag = nodeBlock.Tag.ToString();
                if (!tag.Equals(ControlFlowGraphBuilder<D>.ROOT_SECTION_NAME))
                {
                    name = tag.ToUpper() + ". " + name;
                }
            }
            return name;
        }
    }
}

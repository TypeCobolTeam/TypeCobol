using System;
using System.Collections.Generic;
using System.IO;
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

        /// <summary>
        /// 
        /// </summary>
        /// <param name="block"></param>
        /// <param name="cfg"></param>
        /// <returns></returns>
        protected override bool EmitBasicBlock(BasicBlock<Node, D> block, ControlFlowGraph<Node, D> cfg)
        {
            bool bResult = base.EmitBasicBlock(block, cfg);
            if ((block is ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup))
            {
                ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup group = (ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup)block;
                //we are emitting a sub graph.
                CfgDotFileForNodeGenerator<D> cfgDot = new CfgDotFileForNodeGenerator<D>(cfg);
                cfgDot.FullInstruction = true;
                StringWriter sw = new StringWriter();
                sw.WriteLine("subgraph cluster_" + group.GroupIndex + '{');
                sw.WriteLine("color = blue;");
                sw.WriteLine(string.Format("label = \"{0}\";", ((ControlFlowGraphBuilder<D>.BasicBlockForNode)group.Group.First.Value).Tag));
                cfgDot.Writer = sw;
                cfgDot.DigraphBuilder = new StringBuilder();
                //Emit block starting at the first block.
                LinkedListNode<BasicBlock<Node, D>> first = group.Group.First;
                cfg.DFS(first.Value, (b, g) => cfgDot.EmitBasicBlock(b, g));
                sw.WriteLine(cfgDot.DigraphBuilder.ToString());
                sw.WriteLine('}');
                //Create dashed link to the group
                sw.WriteLine(string.Format("Block{0} -> Block{1} [style=dashed, arrowhead=none]", block.Index, group.Group.First.Value.Index, group.GroupIndex));


                sw.Flush();
                this.Writer.WriteLine(sw.ToString());
            }
            return bResult;
        }
    }
}

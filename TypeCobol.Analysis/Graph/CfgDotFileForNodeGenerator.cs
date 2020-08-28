using System.Collections.Generic;
using System.IO;
using System.Text;
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
        /// Memorized emitted group to avoid infinite recursion.
        /// </summary>
        internal HashSet<int> EmittedGroupIndices
        {
            get;
            set;
        }

        /// <summary>
        /// Emit a basic block, with also handling recursive BasicBlock Groups that can appear for
        /// instance for recursive PERFORM.
        /// </summary>
        /// <param name="block">The BasicBlock to emit</param>
        /// <param name="incomingEdge">The edge that led to the current block</param>
        /// <param name="previousBlock">The previously visited block</param>
        /// <param name="cfg">The target Control Flow Graph that contains the Basic Block</param>
        /// <returns>true</returns>
        protected override bool EmitBasicBlock(BasicBlock<Node, D> block, int incomingEdge, BasicBlock<Node, D> previousBlock, ControlFlowGraph<Node, D> cfg)
        {
            var group = block as ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup;
            if (group != null)
            {
                if (group.IsIterativeGroup && incomingEdge == group.EntryIndexInSuccessors)
                {
                    if (group.IsAfterIterativeGroup)
                    {
                        //For an AFTER iteration group - Draw/Traverse only once the Group
                        //So if the Group has been already entered don't redraw it
                        if (EmittedGroupIndices != null && EmittedGroupIndices.Contains(group.GroupIndex))
                            return false;
                    }
                    else
                    {
                        //Don't follow the iteration edge
                        return false;
                    }
                }
            }

            bool bResult = base.EmitBasicBlock(block, incomingEdge, previousBlock, cfg);
            if (group != null && !block.HasFlag(BasicBlock<Node, D>.Flags.GroupGrafted))
            {
                if (group.IsIterativeGroup && group.IsExplicitIterativeGroup)
                {
                    return bResult;
                }
                if (EmittedGroupIndices == null)
                {
                    EmittedGroupIndices = new HashSet<int>();
                }
                StringWriter sw = new StringWriter();
                if (!EmittedGroupIndices.Contains(group.GroupIndex))
                {
                    EmittedGroupIndices.Add(group.GroupIndex);
                    //we are emitting a sub graph.
                    sw.WriteLine("subgraph cluster_" + group.GroupIndex + '{');
                    sw.WriteLine("color = blue;");
                    if (group.Group.Count > 0)
                    {
                        sw.WriteLine(string.Format("label = \"{0}\";", ((ControlFlowGraphBuilder<D>.BasicBlockForNode)group.Group.First.Value).Tag));
                        CfgDotFileForNodeGenerator<D> cfgDot = new CfgDotFileForNodeGenerator<D>(cfg);
                        cfgDot.EmittedGroupIndices = EmittedGroupIndices;
                        cfgDot.FullInstruction = this.FullInstruction;
                        cfgDot.Writer = sw;
                        cfgDot.DigraphBuilder = new StringBuilder();
                        //Emit block starting at the first block.
                        LinkedListNode<BasicBlock<Node, D>> first = group.Group.First;
                        cfg.DFS(first.Value, cfgDot.EmitBasicBlock);
                        sw.WriteLine(cfgDot.DigraphBuilder.ToString());
                    }
                    sw.WriteLine('}');
                }
                //Create dashed link to the group
                if (group.Group.Count > 0)
                {
                    sw.WriteLine(string.Format("Block{0} -> Block{1} [style=dashed]", block.Index, group.Group.First.Value.Index));
                }
                else
                {
                    sw.WriteLine(string.Format("Block{0} -> \"\" [style=dashed]", block.Index));
                }
                sw.Flush();
                this.Writer.WriteLine(sw.ToString());
            }

            return bResult;
        }
    }
}

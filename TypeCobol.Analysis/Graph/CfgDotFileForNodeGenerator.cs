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
        private readonly HashSet<int> _encounteredBlocks;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">The underlying Control Flow Graph</param>
        public CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg)
            : this(cfg, new HashSet<int>())
        {
            
        }

        /// <summary>
        /// Private constructor. Allows to share the encountered blocks set
        /// between instances of the generator.
        /// </summary>
        /// <param name="cfg">Current CFG to generate.</param>
        /// <param name="encounteredBlocks">Hashset of indices of already encountered blocks.</param>
        private CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg, HashSet<int> encounteredBlocks)
            : base(cfg)
        {
            _encounteredBlocks = encounteredBlocks;
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
            if (_encounteredBlocks.Contains(block.Index))
            {
                return false;
            }

            _encounteredBlocks.Add(block.Index);

            bool bResult = base.EmitBasicBlock(block, incomingEdge, previousBlock, cfg);
            if (block is ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup group && !group.HasFlag(BasicBlock<Node, D>.Flags.GroupGrafted))
            {
                StringWriter sw = new StringWriter();
                if (group.Group.Count > 0)
                {
                    //Generate blocks and edges for subgraph
                    CfgDotFileForNodeGenerator<D> cfgDot = new CfgDotFileForNodeGenerator<D>(cfg, _encounteredBlocks);
                    cfgDot.FullInstruction = this.FullInstruction;
                    cfgDot.BlocksBuilder = new StringBuilder();
                    cfgDot.EdgesBuilder = new StringBuilder();
                    LinkedListNode<BasicBlock<Node, D>> first = group.Group.First;
                    cfg.DFS(first.Value, cfgDot.EmitBasicBlock);
                    string blocks = cfgDot.BlocksBuilder.ToString();
                    string edges = cfgDot.EdgesBuilder.ToString();

                    if (!string.IsNullOrWhiteSpace(blocks))
                    {
                        //Write subgraph
                        sw.WriteLine("subgraph cluster_" + group.GroupIndex + '{');
                        sw.WriteLine("color = blue;");
                        sw.WriteLine($"label = \"{((ControlFlowGraphBuilder<D>.BasicBlockForNode) first.Value).Tag}\";");
                        sw.Write(blocks);
                        sw.WriteLine(edges);
                        sw.WriteLine('}');
                    }

                    //Create dashed link to the group
                    sw.WriteLine("Block{0} -> Block{1} [style=dashed]", block.Index, group.Group.First.Value.Index);
                }
                else
                {
                    //Group is empty, create link to an empty block
                    sw.WriteLine("Block{0} -> \"\" [style=dashed]", block.Index);
                }
                sw.Flush();

                //Include subgraph into parent graph
                this.BlocksBuilder.AppendLine(sw.ToString());
            }

            return bResult;
        }
    }
}

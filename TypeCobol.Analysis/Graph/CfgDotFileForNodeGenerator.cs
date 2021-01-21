using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Report;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// DOT generator for Control Flow Graph base on TypeCobol Nodes 
    /// </summary>
    /// <typeparam name="D"></typeparam>
    public class CfgDotFileForNodeGenerator<D> : ICfgFileGenerator<Node, D>, IReport
    {
        /// <summary>
        /// Escape a text string for the dot format
        /// </summary>
        /// <param name="text">The text string to be escaped</param>
        /// <returns>The escaped string</returns>
        private static string Escape(string text)
        {
            StringBuilder sb = new StringBuilder();
            foreach (char c in text)
            {
                switch (c)
                {
                    case '\\':
                    case '"':
                    case '|':
                    case '<':
                    case '>':
                    case '{':
                    case '}':
                        sb.Append('\\');
                        break;
                    case '\n':
                    case '\r':
                        sb.Append(' ');
                        continue;
                }
                sb.Append(c);
            }
            return sb.ToString();
        }

        private readonly HashSet<int> _encounteredBlocks;
        private readonly StringBuilder _blocksBuffer;
        private readonly StringBuilder _edgesBuffer;
        private ControlFlowGraph<Node, D> _cfg;
        private readonly int _clusterIndex = -1;

        public delegate void BlockEmitted(BasicBlock<Node, D> block, int clusterIndex);
        public event BlockEmitted BlockEmittedEvent;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">Control Flow Graph to represent, can be null and set later through the Generate method.</param>
        public CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg)
            : this(cfg, null)
        {
            
        }

        private CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg, CfgDotFileForNodeGenerator<D> parentGenerator, int clusterIndex = -1)
        {
            this._clusterIndex = clusterIndex;
            if (parentGenerator != null)
            {
                //List of encountered blocks is inherited from parent
                _encounteredBlocks = parentGenerator._encounteredBlocks;
                //In DOT, if an edge belongs to a subgraph, then its target block must be included into that subgraph
                //To avoid unwanted block inclusions, we write all edges at the root level (main digraph)
                _edgesBuffer = parentGenerator._edgesBuffer;
                this.BlockEmittedEvent = parentGenerator.BlockEmittedEvent;
            }
            else
            {
                _encounteredBlocks = new HashSet<int>();
                _edgesBuffer = new StringBuilder();
            }

            //The blocks buffer is per instance
            _blocksBuffer = new StringBuilder();

            _cfg = cfg;
        }

        /// <summary>
        /// Controls the instruction format in generated dot file.
        /// </summary>
        public bool FullInstruction { get; set; }

        /// <summary>
        /// Generate the Control Flow Graph in the given TextWriter
        /// </summary>
        /// <param name="writer">The Writer in which to generate the graph</param>
        /// <param name="cfg">The Control Flow Graph to be Generated</param>
        public void Generate(TextWriter writer, ControlFlowGraph<Node, D> cfg)
        {
            _cfg = cfg;
            Report(writer);
        }

        /// <summary>
        /// Generate the current Control Flow Graph of this generator in the given TextWriter
        /// </summary>
        /// <param name="writer">Output TextWriter</param>
        public void Report(TextWriter writer, CompilationUnit unit = null)
        {
            //Reset state
            _encounteredBlocks.Clear();
            _blocksBuffer.Clear();
            _edgesBuffer.Clear();

            //Nothing to generate
            if (_cfg == null) return;

            //Write header
            writer.WriteLine("digraph Cfg {");
            if (_cfg.HasFlag(ControlFlowGraph<Node, D>.Flags.Compound))
            {
                writer.WriteLine("compound=true;");
            }

            //Blocks properties
            writer.WriteLine("node [");
            writer.WriteLine("shape = \"record\"");
            writer.WriteLine("]");
            writer.WriteLine("");

            //Edges properties
            writer.WriteLine("edge [");
            writer.WriteLine("arrowtail = \"empty\"");
            writer.WriteLine("]");

            //Run DFS on the flow graph, with the emitter callback method.
            _cfg.DFS(EmitBlock);
            writer.Write(_blocksBuffer.ToString());
            writer.WriteLine(_edgesBuffer.ToString());

            //Close graph
            writer.WriteLine("}");
            writer.Flush();
        }

        private bool EmitBlock(BasicBlock<Node, D> block, int incomingEdge, BasicBlock<Node, D> predecessorBlock, ControlFlowGraph<Node, D> cfg)
        {
            if (_encounteredBlocks.Contains(block.Index)) return false;
            _encounteredBlocks.Add(block.Index);

            //Write block to blocks buffer
            WriteBlock(block);

            //Special treatment for group blocks
            if (block is ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup group && !group.HasFlag(BasicBlock<Node, D>.Flags.GroupGrafted))
            {
                StringWriter writer = new StringWriter();
                if (group.Group.Count > 0)
                {
                    //Generate blocks for subgraph using a nested generator
                    var subgraphGenerator = new CfgDotFileForNodeGenerator<D>(_cfg, this, group.GroupIndex);
                    subgraphGenerator.FullInstruction = this.FullInstruction;
                    BasicBlock<Node, D> first = group.Group.First.Value;
                    _cfg.DFS(first, subgraphGenerator.EmitBlock);
                    string blocks = subgraphGenerator._blocksBuffer.ToString();

                    System.Diagnostics.Debug.Assert(blocks != null);
                    if (blocks != string.Empty)
                    {
                        //Write subgraph
                        writer.WriteLine("subgraph cluster_" + group.GroupIndex + '{');
                        writer.WriteLine("color = blue;");
                        writer.WriteLine($"label = \"{((ControlFlowGraphBuilder<D>.BasicBlockForNode) first).Tag}\";");
                        writer.WriteLine(blocks);
                        writer.WriteLine('}');
                    }

                    //Create dashed link to the group
                    writer.WriteLine("Block{0} -> Block{1} [style=dashed]", block.Index, first.Index);
                }
                else
                {
                    //Group is empty, create link to an empty block
                    writer.WriteLine("Block{0} -> \"\" [style=dashed]", block.Index);
                }
                writer.Flush();

                //Include subgraph into parent graph
                _blocksBuffer.AppendLine(writer.ToString());
            }

            //Write edges to edges buffer
            foreach (var edge in block.SuccessorEdges)
            {
                System.Diagnostics.Debug.Assert(edge >= 0 && edge < _cfg.SuccessorEdges.Count);
                _edgesBuffer.AppendLine($"Block{block.Index} -> Block{_cfg.SuccessorEdges[edge].Index}");
            }
            if (BlockEmittedEvent != null)
                BlockEmittedEvent(block, _clusterIndex);
            return true;
        }

        private void WriteBlock(BasicBlock<Node, D> block)
        {
            //Block title
            _blocksBuffer.AppendLine($"Block{block.Index} [");
            _blocksBuffer.Append("label = \"{");
            _blocksBuffer.Append(BlockName(block));
            _blocksBuffer.Append("|");

            //Print all instructions inside the block.
            foreach (var i in block.Instructions)
            {
                _blocksBuffer.Append(InstructionToString(i));
                _blocksBuffer.Append("\\l");
            }

            //Close block
            _blocksBuffer.AppendLine("}\"");
            _blocksBuffer.AppendLine("]");
        }

        /// <summary>
        /// Get the dot format name of a block.
        /// </summary>
        /// <param name="block">The block to get the dot format name.</param>
        /// <returns>The block's name</returns>
        private string BlockName(BasicBlock<Node, D> block)
        {
            string name = block.HasFlag(BasicBlock<Node, D>.Flags.Start)
                ? "START"
                : block.HasFlag(BasicBlock<Node, D>.Flags.End)
                    ? "END"
                    : "Block" + block.Index;
            ControlFlowGraphBuilder<D>.BasicBlockForNode nodeBlock = (ControlFlowGraphBuilder<D>.BasicBlockForNode) block;
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
        /// Get the string representing an instruction.
        /// </summary>
        /// <param name="instruction">The instruction to get the string representation.</param>
        private string InstructionToString(Node instruction)
        {
            return instruction?.CodeElement == null
                ? "<null>"
                : FullInstruction
                    ? Escape(instruction.CodeElement.SourceText)
                    : System.Enum.GetName(typeof(CodeElementType), instruction.CodeElement.Type);
        }
    }
}

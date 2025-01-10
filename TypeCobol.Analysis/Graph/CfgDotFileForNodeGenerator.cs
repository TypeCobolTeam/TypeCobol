using System.Diagnostics;
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
                        sb.Append("  "); // Lines breaks translate to two spaces regardless of the platform
                        continue;
                    case '\r':
                        // Ignore CR
                        continue;
                }
                sb.Append(c);
            }
            return sb.ToString();
        }

        /// <summary>
        /// Set of encountered blocks during one generation, each block is identified by its index.
        /// Instantiated once in the constructor of root generator, then shared with all of its children.
        /// Cleared each time a new generation is requested.
        /// </summary>
        private readonly HashSet<int> _encounteredBlocks;

        /// <summary>
        /// Buffer for blocks. Each generator has its own instance. This buffer is created as soon as
        /// we have the format parameters (FormatProvider and NewLine):
        /// - for root generator that is when the generation is requested, using the supplied TextWriter
        /// - for children generators, this is in the constructor having the parent generator as argument
        /// </summary>
        private StringWriter _blocksBuffer;

        /// <summary>
        /// Buffer for edges. This buffer is shared across root generator and its children.
        /// Like block generator, it is parameterized using the target writer properties.
        /// </summary>
        private StringWriter _edgesBuffer;

        /// <summary>
        /// The current CFG to generate in DOT language.
        /// </summary>
        private ControlFlowGraph<Node, D> _cfg;

        /// <summary>
        /// The identifier of current graph or subgraph being generated, a new identifier is generated for each block group coming from the CFG.
        /// </summary>
        private readonly int _clusterIndex;

        public delegate void BlockEmitted(BasicBlock<Node, D> block, int clusterIndex);
        public event BlockEmitted BlockEmittedEvent;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">Control Flow Graph to represent, can be null and set later through the Generate method.</param>
        public CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg)
            : this(cfg, null, -1)
        {
            
        }

        private CfgDotFileForNodeGenerator(ControlFlowGraph<Node, D> cfg, CfgDotFileForNodeGenerator<D> parentGenerator, int clusterIndex)
        {
            if (parentGenerator != null)
            {
                //List of encountered blocks is inherited from parent
                _encounteredBlocks = parentGenerator._encounteredBlocks;
                //Separate block writer but keep params from parent
                _blocksBuffer = new StringWriter(parentGenerator._blocksBuffer.FormatProvider) { NewLine = parentGenerator._blocksBuffer.NewLine };
                //In DOT, if an edge belongs to a subgraph, then its target block must be included into that subgraph
                //To avoid unwanted block inclusions, we write all edges at the root level (main digraph)
                _edgesBuffer = parentGenerator._edgesBuffer;

                this.BlockEmittedEvent = parentGenerator.BlockEmittedEvent;
            }
            else
            {
                _encounteredBlocks = new HashSet<int>();
                // Instances are created when generation is requested, using the parameters from target writer
                _blocksBuffer = null;
                _edgesBuffer = null;
            }

            _cfg = cfg;
            _clusterIndex = clusterIndex;
        }

        /// <summary>
        /// Controls the instruction format in generated dot file.
        /// </summary>
        public bool FullInstruction { get; init; }

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
            if (_blocksBuffer != null || _edgesBuffer != null)
            {
                throw new InvalidOperationException("Cannot generate subgraph using arbitrary parent graph generator ! Generate using root graph only !");
            }

            if (_cfg == null)
            {
                //Nothing to generate
                return;
            }

            //Initialize buffers with target writer parameters and reset encountered blocks
            _encounteredBlocks.Clear();
            _blocksBuffer = new StringWriter(writer.FormatProvider) { NewLine = writer.NewLine };
            _edgesBuffer = new StringWriter(writer.FormatProvider) { NewLine = writer.NewLine }; 

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

            //Reset writers
            _blocksBuffer = null;
            _edgesBuffer = null;
        }

        private bool EmitBlock(BasicBlock<Node, D> block, int incomingEdge, BasicBlock<Node, D> predecessorBlock, ControlFlowGraph<Node, D> cfg)
        {
            if (!_encounteredBlocks.Add(block.Index)) return false;

            //Write block to blocks buffer
            WriteBlock(block);

            //Special treatment for group blocks
            if (block is ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup group && !group.HasFlag(BasicBlock<Node, D>.Flags.GroupGrafted))
            {
                if (group.Group.Count > 0)
                {
                    //Generate blocks for subgraph using a nested generator
                    var subgraphGenerator = new CfgDotFileForNodeGenerator<D>(_cfg, this, group.GroupIndex) { FullInstruction = this.FullInstruction };
                    BasicBlock<Node, D> first = group.Group.First.Value;
                    _cfg.DFS(first, subgraphGenerator.EmitBlock);
                    string blocks = subgraphGenerator._blocksBuffer.ToString();

                    Debug.Assert(blocks != null);
                    if (blocks != string.Empty)
                    {
                        //Include subgraph into parent graph
                        _blocksBuffer.WriteLine("subgraph cluster_" + group.GroupIndex + '{');
                        _blocksBuffer.WriteLine("color = blue;");
                        _blocksBuffer.WriteLine($"label = \"{((ControlFlowGraphBuilder<D>.BasicBlockForNode) first).Tag}\";");
                        _blocksBuffer.WriteLine(blocks);
                        _blocksBuffer.WriteLine('}');
                    }

                    //Create dashed link to the group
                    _blocksBuffer.WriteLine("Block{0} -> Block{1} [style=dashed]", block.Index, first.Index);
                }
                else
                {
                    //Group is empty, create link to an empty block
                    _blocksBuffer.WriteLine("Block{0} -> \"\" [style=dashed]", block.Index);
                }

                _blocksBuffer.WriteLine();
            }

            //Write edges to edges buffer
            foreach (var edge in block.SuccessorEdges)
            {
                Debug.Assert(edge >= 0 && edge < _cfg.SuccessorEdges.Count);
                _edgesBuffer.WriteLine($"Block{block.Index} -> Block{_cfg.SuccessorEdges[edge].Index}");
            }
            if (BlockEmittedEvent != null)
                BlockEmittedEvent(block, _clusterIndex);
            return true;
        }

        private void WriteBlock(BasicBlock<Node, D> block)
        {
            //Block title
            _blocksBuffer.WriteLine($"Block{block.Index} [");
            _blocksBuffer.Write("label = \"{");
            _blocksBuffer.Write(BlockName(block));
            _blocksBuffer.Write("|");

            //Print all instructions inside the block.
            foreach (var i in block.Instructions)
            {
                _blocksBuffer.Write(InstructionToString(i));
                _blocksBuffer.Write("\\l");
            }

            //Close block
            _blocksBuffer.WriteLine("}\"");
            _blocksBuffer.WriteLine("]");
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
                    : Enum.GetName(typeof(CodeElementType), instruction.CodeElement.Type);
        }
    }
}

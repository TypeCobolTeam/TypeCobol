using System.IO;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Report;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// GraphViz Dot file generator for a Control Flow Graph
    /// </summary>
    /// <typeparam name="N"></typeparam>
    /// <typeparam name="D"></typeparam>
    public class CfgDotFileGenerator<N, D> : ICfgFileGenerator<N, D>, IReport
    {
        /// <summary>
        /// The underlying Control Flow Graph
        /// </summary>
        public ControlFlowGraph<N, D> Cfg { get; set; }

        /// <summary>
        /// The blocks buffer
        /// </summary>
        protected StringBuilder BlocksBuilder;

        /// <summary>
        /// The edges buffer
        /// </summary>
        protected StringBuilder EdgesBuilder;

        /// <summary>
        /// Get the string representing an instruction.
        /// </summary>
        /// <param name="instruction">The instruction to get the string representation.</param>
        protected virtual string InstructionToString(N instruction)
        {
            return instruction == null ? "<null>" : instruction.ToString();
        }

        /// <summary>
        /// Get the dot format name of a block.
        /// </summary>
        /// <param name="block">The block to get the dot format name.</param>
        /// <returns>The block's name</returns>
        protected virtual string BlockName(BasicBlock<N, D> block)
        {
            string name = block.HasFlag(BasicBlock<N, D>.Flags.Start) ? "START" :
                block.HasFlag(BasicBlock<N, D>.Flags.End) ? "END" : ("Block" + block.Index);
            return name;
        }

        /// <summary>
        /// Call back function for emitting a BasicBlock.
        /// </summary>
        /// <param name="block">Current block</param>
        /// <param name="incomingEdge">Incoming edge</param>
        /// <param name="previousBlock">Previous block</param>
        /// <param name="cfg">Current graph being traversed</param>
        /// <returns></returns>
        protected virtual bool EmitBasicBlock(BasicBlock<N, D> block, int incomingEdge, BasicBlock<N, D> previousBlock, ControlFlowGraph<N, D> cfg)
        {
            //Write block to blocks buffer
            BlocksBuilder.AppendLine($"Block{block.Index} [");
            BlocksBuilder.Append("label = \"{");
            BlocksBuilder.Append(BlockName(block));
            BlocksBuilder.Append("|");

            //Print all instructions inside the block.
            foreach (var i in block.Instructions)
            {
                BlocksBuilder.Append(InstructionToString(i));
                BlocksBuilder.Append("\\l");
            }

            BlocksBuilder.AppendLine("}\"");
            BlocksBuilder.AppendLine("]");

            //Write edges to edges buffer
            foreach (var edge in block.SuccessorEdges)
            {
                System.Diagnostics.Debug.Assert(edge >= 0 && edge < cfg.SuccessorEdges.Count);
                EdgesBuilder.AppendLine($"Block{block.Index} -> Block{cfg.SuccessorEdges[edge].Index}");
            }

            return true;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">The underlying Control Flow Graph</param>
        public CfgDotFileGenerator(ControlFlowGraph<N, D> cfg)
        {
            this.Cfg = cfg;
        }

        public void Generate(TextWriter writer, ControlFlowGraph<N, D> cfg)
        {
            this.Cfg = cfg;
            if (Cfg != null)
            {
                BlocksBuilder = new StringBuilder();
                EdgesBuilder = new StringBuilder();
                writer.WriteLine("digraph Cfg {");
                if (cfg.HasFlag(ControlFlowGraph<N, D>.Flags.Compound))
                {
                    writer.WriteLine("compound=true;");
                }

                //node properties
                writer.WriteLine("node [");
                writer.WriteLine("shape = \"record\"");
                writer.WriteLine("]");
                writer.WriteLine("");

                //edges properties
                writer.WriteLine("edge [");
                writer.WriteLine("arrowtail = \"empty\"");
                writer.WriteLine("]");

                //Run DFS on the flow graph, with the emitter callback method.
                Cfg.DFS(EmitBasicBlock);
                writer.Write(BlocksBuilder.ToString());
                writer.WriteLine(EdgesBuilder.ToString());
                writer.WriteLine("}");
            }

            writer.Flush();
        }

        public void Report(TextWriter writer, CompilationUnit unit = null)
        {
            Generate(writer, Cfg);
        }

        /// <summary>
        /// Escape a text string for the dot format
        /// </summary>
        /// <param name="text">The text string to be escaped</param>
        /// <returns>The escaped string</returns>
        public static string Escape(string text)
        {
            StringBuilder sb = new StringBuilder();
            foreach(char c in text)
            {
                switch(c)
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
    }
}

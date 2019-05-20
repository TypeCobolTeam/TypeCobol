using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Report;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// Graphviz Dot file generator for a Control Flow Graph
    /// </summary>
    /// <typeparam name="N"></typeparam>
    /// <typeparam name="D"></typeparam>
    public class CfgDotFileGenerator<N, D> : AbstractReport, ICfgFileGenerator<N, D>
    {
        /// <summary>
        /// The underlying Control Flow Graph
        /// </summary>
        public ControlFlowGraph<N, D> Cfg
        {
            get;
            set;
        }

        /// <summary>
        /// The Current Writer.
        /// </summary>
        private TextWriter Writer
        {
            get;
            set;
        }

        /// <summary>
        /// The Digraph buffer
        /// </summary>
        private StringBuilder DigraphBuilder;

        /// <summary>
        /// Get the string representing an instruction.
        /// </summary>
        /// <param name="instruction">The instruction to get the string representation.</param>
        protected virtual String InstructionToString(N instruction)
        {
            return instruction == null ? "<null>" : instruction.ToString();
        }

        /// <summary>
        /// Call back function for emitting a BasicBlock.
        /// </summary>
        /// <param name="block"></param>
        /// <param name="cfg"></param>
        private bool EmitBasicBlock(BasicBlock<N, D> block, ControlFlowGraph<N, D> cfg)
        {
            Writer.WriteLine(string.Format("Block{0} [", block.Index));
            StringBuilder sb = new StringBuilder("label = \"{");
            sb.Append(block.HasFlag(BasicBlock<N, D>.Flags.Start) ? "START" :
                block.HasFlag(BasicBlock<N, D>.Flags.End) ? "END" : ("Block" + block.Index));
            sb.Append("|");

            //Print all instructions inside the block.
            foreach(var i in block.Instructions)
            {
                sb.Append(InstructionToString(i));
                sb.Append("\\l");
            }
            sb.Append("\"}");
            Writer.WriteLine("]");

            //Emit the digraph
            foreach(var edge in block.SuccessorEdges)
            {
                DigraphBuilder.AppendLine(string.Format("Block{0} -> Block{1}", block.Index, edge));
            }

            return true;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg"></param>
        public CfgDotFileGenerator()
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">The underlying Control Flow Graph</param>
        public CfgDotFileGenerator(ControlFlowGraph<N, D> cfg)
        {
        }

        public void Generate(TextWriter writer, ControlFlowGraph<N, D> cfg)
        {
            //Set the current writer
            this.Writer = writer;
            this.Cfg = cfg;
            if (Cfg != null)
            {
                DigraphBuilder = new StringBuilder();
                Writer.WriteLine("digraph Cfg {");
                Writer.WriteLine("node [");
                Writer.WriteLine("shape = \"record\"");
                Writer.WriteLine("]");
                Writer.WriteLine("");

                Writer.WriteLine("edge [");
                Writer.WriteLine("arrowtail = \"empty\"");
                Writer.WriteLine("]");

                //Run DFS on the flow graph, with the emiter callback method.
                Cfg.DFS(EmitBasicBlock);
                Writer.WriteLine(DigraphBuilder.ToString());
                Writer.WriteLine("}");
            }
        }

        public override void Report(TextWriter writer)
        {
            Generate(writer, Cfg);
        }
    }
}


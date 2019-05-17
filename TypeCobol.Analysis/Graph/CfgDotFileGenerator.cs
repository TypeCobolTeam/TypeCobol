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
        /// Call back function for emitting a BasicBlock.
        /// </summary>
        /// <param name="block"></param>
        /// <param name="cfg"></param>
        private bool EmitBasicBlock(BasicBlock<N, D> block, ControlFlowGraph<N, D> cfg)
        {
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
                Writer.WriteLine("digraph G {");
                //Run DFS on the flow graph, with the emiter callback method.
                Cfg.DFS(EmitBasicBlock);
                Writer.WriteLine("}");
            }
        }

        public override void Report(TextWriter writer)
        {
            Generate(writer, Cfg);
        }
    }
}


using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Graph;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Data Flow Graph Builder for Data Flow Analysis
    /// </summary>
    /// <typeparam name="N">Instruction type</typeparam>
    /// <typeparam name="D">Data info type</typeparam>
    public class DataFlowGraphBuilder<N, D>
    {
        /// <summary>
        /// The underlying Control Flow Graph to be added with Data Flow Information. 
        /// </summary>
        public ControlFlowGraph<DfaBasicBlockInfo<N>, D> Cfg
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">Underlying Control Flow Graph</param>
        public DataFlowGraphBuilder(ControlFlowGraph<DfaBasicBlockInfo<N>, D> cfg)
        {
            this.Cfg = cfg;
        }
    }
}

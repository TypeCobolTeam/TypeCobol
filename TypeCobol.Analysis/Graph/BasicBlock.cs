using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// A Block of instruction of which the first instruction can be reached by an explicit control flow.
    /// </summary>
    /// <typeparam name="N">The Type Variable of an instruction</typeparam>
    /// <typeparam name="D">The Type Variable for any extra Data information</typeparam>
    public class BasicBlock<N, D>
    {
        /// <summary>
        /// Instructions making up this block.
        /// </summary>
        public LinkedList<N> Instructions
        {
            get;
            internal set;
        }

        /// <summary>
        /// Data Information of this block.
        /// </summary>
        public D Data
        {
            get;
            set;
        }

        /// <summary>
        /// The first edge that leaves this block. The edges are a contiguous sublist of the SuccessorEdges list of the ControlFlowGraph that contains this block.
        /// </summary>
        public int FirstSuccessorEdge
        {
            get;
            set;
        }

        /// <summary>
        /// The number of edges that leave this block. The edges are a contiguous sublist of the the SuccessorEdges list of the ControlFlowGraph that contains this block.
        /// </summary>
        public int SuccessorCount
        {
            get;
            set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public BasicBlock()
        {
        }
    }
}

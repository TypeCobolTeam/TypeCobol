using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// A set of basic blocks, each of which has a list of successor blocks and some other information.
    /// Each block consists of a list of instructions, each of which can point to previous instructions that compute the operands it consumes.
    /// </summary>
    /// <typeparam name="N"></typeparam>
    /// <typeparam name="D"></typeparam>
    public class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// Root blocks. Usually it is a singleton it is the first block in the program, but alos on Exception handlers.
        /// </summary>
        public List<BasicBlock<N, D>> RootBlocks
        {
            get;
            internal set;
        }

        /// <summary>
        /// All blocks. A list of basic blocks in the graph.
        /// </summary>
        public List<BasicBlock<N, D>> AllBlocks
        {
            get;
            internal set;
        }

        /// <summary>
        /// A map from Node to corresponding basic block.
        /// </summary>
        public Dictionary<N, BasicBlock<N,D>> BlockFor
        {
            get;
            internal set;
        }

        /// <summary>
        /// The list of all Successor edges. The successor list for each basic block is a sublist of this list
        /// </summary>
        public List<BasicBlock<N, D>> SuccessorEdges
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Node of the program for which this control Flow Graph has been created.
        /// </summary>
        public N ProgramNode
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Node of the procedure for which this control Flow Graph has been created.
        /// </summary>
        public N ProcedureNode
        {
            get;
            internal set;
        }

        /// <summary>
        /// Intialize the construction of the Control Flow Graph.
        /// </summary>
        internal virtual void Initialize()
        {
            BlockFor = new Dictionary<N, BasicBlock<N, D>>();
            AllBlocks = new List<BasicBlock<N, D>>();
            RootBlocks = new List<BasicBlock<N, D>>();
            SuccessorEdges = new List<BasicBlock<N, D>>();
        }

        /// <summary>
        /// All basic blocks that can be reached via control flow out of the given basic block.
        /// </summary>
        /// <param name="basicBlock">The basic block to get the successors</param>
        /// <returns>The sublist of successors</returns>
        public List<BasicBlock<N,D>> SuccessorsFor(BasicBlock<N, D> basicBlock)
        {
            System.Diagnostics.Contracts.Contract.Requires(basicBlock != null);
            if (basicBlock.FirstSuccessorEdge + basicBlock.SuccessorCount > this.SuccessorEdges.Count)
                throw new InvalidOperationException();//Can happen if the basic bloc does not belong to this graph
            System.Diagnostics.Contracts.Contract.Assume(basicBlock.FirstSuccessorEdge >= 0);
            System.Diagnostics.Contracts.Contract.Assume(basicBlock.SuccessorCount >= 0);
            return this.SuccessorEdges.GetRange(basicBlock.FirstSuccessorEdge, basicBlock.SuccessorCount);
        }
    }
}


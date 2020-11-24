using System;
using System.Collections.Generic;

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
        /// BasicBlock callback type.
        /// </summary>
        /// <param name="block">The current block.</param>
        /// <param name="incomingEdge">Incoming edge that led to the current block (i.e. block equals to cfg.SuccessorEdges[incomingEdge]).
        /// -1 for a root block.</param>
        /// <param name="predecessorBlock">Previous block. null if current block is a root block.</param>
        /// <param name="cfg">The Control Flow Graph in which the basic Block belongs to.</param>
        /// <returns>true to continue the traversal, false to stop.</returns>
        public delegate bool BasicBlockCallback(BasicBlock<N, D> block, int incomingEdge, BasicBlock<N, D> predecessorBlock, ControlFlowGraph<N, D> cfg);

        /// <summary>
        /// Flag on a Cfg.
        /// </summary>
        [Flags]
        public enum Flags : uint
        {
            Compound = 0x01 << 0,       //Flag to indicate that this Cfg graph has subgraphs.
        }

        /// <summary>
        /// Symbol Flags.
        /// </summary>
        public Flags Flag
        {
            get;
            private set;
        }

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        internal virtual void SetFlag(Flags flag, bool value)
        {
            this.Flag = value ? this.Flag | flag
                              : this.Flag & ~flag;
        }

        /// <summary>
        /// Determines if the given flag is set.
        /// </summary>
        /// <param name="flag">The flag to be tested</param>
        /// <returns>true if yes, false otherwise.</returns>
        public bool HasFlag(Flags flag)
        {
            return (this.Flag & flag) != 0;
        }

        /// <summary>
        /// Terminals blocks, those blocks that don't have successors.
        /// </summary>
        public LinkedList<BasicBlock<N, D>> TerminalsBlocks
        {
            get;
            internal set;
        }


        /// <summary>
        /// Root block of this graph. This is the first block accessed in the program.
        /// </summary>
        public BasicBlock<N, D> RootBlock
        {
            get;
            internal set;
        }

        /// <summary>
        /// All blocks. A list of basic blocks in the graph.
        /// </summary>
        public List<BasicBlock<N, D>> AllBlocks { get; private set; }

        /// <summary>
        /// A map from Node to corresponding basic block.
        /// </summary>
        public Dictionary<N, BasicBlock<N,D>> BlockFor { get; private set; }

        /// <summary>
        /// The list of all Successor edges. The successor list for each basic block is a sublist of this list
        /// </summary>
        public List<BasicBlock<N, D>> SuccessorEdges { get; private set; }

        /// <summary>
        /// The list of all Predecessor edges. The predecessor list for each basic block is a sublist of this list
        /// </summary>
        public List<BasicBlock<N, D>> PredecessorEdges { get; private set; }

        /// <summary>
        /// The parent graph of this graph.
        /// </summary>
        public ControlFlowGraph<N, D> ParentGraph { get; }

        /// <summary>
        /// The collection of nested graphs of this graph.
        /// </summary>
        public List<ControlFlowGraph<N, D>> NestedGraphs { get; private set; }

        /// <summary>
        /// The Node of the program or function for which this control Flow Graph has been created.
        /// </summary>
        public N ProgramOrFunctionNode { get; }

        /// <summary>
        /// The Node of the procedure division containing the statements represented by this graph.
        /// </summary>
        public N ProcedureDivisionNode { get; private set; }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="programOrFunctionNode">The program of function node of the graph.</param>
        /// <param name="parentGraph">The parent graph if any.</param>
        internal ControlFlowGraph(N programOrFunctionNode, ControlFlowGraph<N, D> parentGraph)
        {
            ProgramOrFunctionNode = programOrFunctionNode;
            ParentGraph = parentGraph;
            if (parentGraph != null)
            {
                if (parentGraph.NestedGraphs == null)
                    parentGraph.NestedGraphs = new List<ControlFlowGraph<N, D>>();
                parentGraph.NestedGraphs.Add(this);
            }
            //At least empty if not Initialized
            AllBlocks = new List<BasicBlock<N, D>>(0);
        }

        /// <summary>
        /// Initialize the construction of the Control Flow Graph.
        /// </summary>
        internal virtual void Initialize(N procedureDivisionNode)
        {
            ProcedureDivisionNode = procedureDivisionNode;
            RootBlock = null;
            AllBlocks = new List<BasicBlock<N, D>>();
            BlockFor = new Dictionary<N, BasicBlock<N, D>>();
            SuccessorEdges = new List<BasicBlock<N, D>>();
        }

        /// <summary>
        /// Determine if this Cfg has already been initialized
        /// (i.e. encountered a PROCEDURE DIVISION node while building).
        /// </summary>
        public bool IsInitialized => ProcedureDivisionNode != null;

        /// <summary>
        /// Set up the Predecessor Edges list starting from the root block.	
        /// </summary>
        public void SetupPredecessorEdgesFromRoot()
        {
            if (RootBlock != null)
            {
                SetupPredecessorEdgesFromStart(RootBlock);
            }
        }

        /// <summary>
        /// Set up the Predecessor Edges list starting from a given start block.	
        /// </summary>	
        /// <param name="start">The start block.</param>	
        private void SetupPredecessorEdgesFromStart(BasicBlock<N, D> start)
        {
            if (this.PredecessorEdges != null || this.SuccessorEdges == null)
                return;

            this.TerminalsBlocks = new LinkedList<BasicBlock<N, D>>();
            this.PredecessorEdges = new List<BasicBlock<N, D>>(this.SuccessorEdges.Count);

            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            Stack<BasicBlock<N, D>> stack = new Stack<BasicBlock<N, D>>();
            stack.Push(start);
            while (stack.Count > 0)
            {
                BasicBlock<N, D> block = stack.Pop();
                if (discovered.Get(block.Index))
                    continue;
                discovered.Set(block.Index, true);

                if (block.PredecessorEdges == null)
                {
                    block.PredecessorEdges = new List<int>(0);
                }

                if (block.SuccessorEdges.Count == 0)
                {
                    TerminalsBlocks.AddLast(block);
                }
                else
                {
                    int predIndex = -1;
                    foreach (int successor in block.SuccessorEdges)
                    {
                        BasicBlock<N, D> successorBlock = SuccessorEdges[successor];
                        stack.Push(successorBlock);
                        if (successorBlock.PredecessorEdges == null)
                        {
                            successorBlock.PredecessorEdges = new List<int>();
                        }

                        if (predIndex == -1)
                        {
                            predIndex = this.PredecessorEdges.Count;
                            this.PredecessorEdges.Add(block);
                        }

                        successorBlock.PredecessorEdges.Add(predIndex);
                    }
                }
            }
        }

        /// <summary>
        /// Clear previous analysis data for all blocks.
        /// </summary>
        public void ResetAllBlockData()
        {
            foreach (var block in AllBlocks)
            {
                block.Data = default;
            }
        }

        /// <summary>
        /// DFS Depth First Search implementation
        /// </summary>
        /// <param name="block">The current block</param>
        /// <param name="incomingEdge">Incoming edge that led to the current block</param>
        /// <param name="predecessorBlock">Previous block.</param>
        /// <param name="discovered">Array of already discovered nodes</param>
        /// <param name="callback">CallBack function</param>
        private void DFS(BasicBlock<N, D> block, int incomingEdge, BasicBlock<N, D> predecessorBlock, System.Collections.BitArray discovered, BasicBlockCallback callback)
        {
            discovered[block.Index] = true;

            if (!callback(block, incomingEdge, predecessorBlock, this))
                return;//Means stop

            foreach (var edge in block.SuccessorEdges)
            {
                var nextBlock = SuccessorEdges[edge];
                if (!discovered[nextBlock.Index])
                {
                    DFS(nextBlock, edge, block, discovered, callback);
                }
            }
        }

        /// <summary>
        /// DFS Depth First Search implementation
        /// </summary>
        /// <param name="startBlock">The start block.</param>
        /// <param name="callback">CallBack function</param>
        public void DFS(BasicBlock<N, D> startBlock, BasicBlockCallback callback)
        {
            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            //Incoming edge => -1, Previous block => null
            DFS(startBlock, -1, null, discovered, callback);
        }

        /// <summary>
        /// DFS Depth First Search implementation.
        /// </summary>
        /// <param name="callback">CallBack function</param>
        public void DFS(BasicBlockCallback callback)
        {
            if (RootBlock != null)
                DFS(RootBlock, callback);
        }
    }
}

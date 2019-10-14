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
        /// BasicBlock calllback type.
        /// </summary>
        /// <param name="block">The BasicBlock</param>
        /// <param name="cfg">The Control Flow Graph in wich the basic Block belongs to.</param>
        /// <returns>true if ok, false otherwise</returns>
        public delegate bool BasicBlockCallback(BasicBlock<N, D> block, ControlFlowGraph<N, D> cfg);

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
            internal set;
        }

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        internal virtual void SetFlag(Flags flag, bool value)
        {
            this.Flag = value ? (Flags)(this.Flag | flag)
                              : (Flags)(this.Flag & ~flag);
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
        /// Root blocks. Usually it is a singleton it is the first block in the program, but alos on Exception handlers.
        /// </summary>
        public List<BasicBlock<N, D>> RootBlocks
        {
            get;
            internal set;
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
        /// The list of all Predeccessor edges. The predeccessor list for each basic block is a sublist of this list
        /// </summary>
        public List<BasicBlock<N, D>> PredecessorEdges
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
        /// Determine if this Cfg is entered in its Procedure.
        /// </summary>
        public bool IsInProcedure => ProcedureNode != null;

        /// <summary>
        /// All basic blocks that can be reached via control flow out of the given basic block.
        /// </summary>
        /// <param name="basicBlock">The basic block to get the successors</param>
        /// <returns>The sublist of successors</returns>
        public List<BasicBlock<N,D>> SuccessorsFor(BasicBlock<N, D> basicBlock)
        {
            System.Diagnostics.Contracts.Contract.Requires(basicBlock != null);
            System.Diagnostics.Contracts.Contract.Assume(basicBlock.SuccessorEdges != null);
            List<BasicBlock<N, D>> result = new List<BasicBlock<N, D>>();
            foreach(var n in basicBlock.SuccessorEdges)
            {
                result.Add(SuccessorEdges[n]);
            }
            return result;
        }

        /// <summary>
        /// Set up the Precessor Edges list from the start block.
        /// </summary>
        public void SetupPredecessorEdgesFromStart()
        {
            if (AllBlocks.Count > 0)
                SetupPredecessorEdgesFromRoot(AllBlocks[0]);
        }
        /// <summary>
        /// Set up the Precessor Edges list from a root block.
        /// </summary>
        /// <param name="root">The Root block</param>
        public void SetupPredecessorEdgesFromRoot(BasicBlock<N, D> root)
        {
            if (this.PredecessorEdges != null || this.SuccessorEdges == null)
                return;
            this.TerminalsBlocks = new LinkedList<BasicBlock<N, D>>();
            this.PredecessorEdges = new List<BasicBlock<N, D>>(this.SuccessorEdges.Count);
            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            Stack<BasicBlock<N, D>> stack = new Stack<BasicBlock<N, D>>();
            stack.Push(root);
            while(stack.Count > 0)
            {
                BasicBlock < N, D > block = stack.Pop();
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
        /// Set up the Precessor Edges list, and do it for all blocks.
        /// </summary>
        public void SetupPredecessorEdges()
        {
            if (this.PredecessorEdges != null || this.SuccessorEdges == null)
                return;
            this.TerminalsBlocks = new LinkedList<BasicBlock<N, D>>();
            this.PredecessorEdges = new List<BasicBlock<N, D>>(this.SuccessorEdges.Count);
            foreach(BasicBlock<N, D> block in AllBlocks)
            {
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
        /// DFS Depth First Search implementation
        /// </summary>
        /// <param name="block">The current block</param>
        /// <param name="discovered">Array of already discovered nodes</param>
        /// <param name="callback">CallBack function</param>
        internal void DFS(BasicBlock<N, D> block, System.Collections.BitArray discovered, BasicBlockCallback callback)
        {
            discovered[block.Index] = true;
            if (!callback(block, this))
                return;//Means stop
            foreach (var edge in block.SuccessorEdges)
            {
                if (!discovered[SuccessorEdges[edge].Index])
                {
                    DFS(SuccessorEdges[edge], discovered, callback);
                }
            }
        }

        /// <summary>
        /// DFS Depth First Search implementation
        /// </summary>
        /// <param name="rootBlock">The root block.</param>
        /// <param name="callback">CallBack function</param>
        public void DFS(BasicBlock<N, D> rootBlock, BasicBlockCallback callback)
        {
            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            DFS(rootBlock, discovered, callback);
        }

        /// <summary>
        /// DFS Depth First Search implementation.
        /// </summary>
        /// <param name="callback">CallBack function</param>
        public void DFS(BasicBlockCallback callback)
        {
            foreach(var root in RootBlocks)
            { 
                DFS(root, callback);
            }
        }

        /// <summary>
        /// DFSInverse Depth First Search implementation using predecessors edge.
        /// </summary>
        /// <param name="block">The current block</param>
        /// <param name="discovered">Array of already discovered nodes</param>
        /// <param name="callback">CallBack function</param>
        internal void DFSInverse(BasicBlock<N, D> block, System.Collections.BitArray discovered, BasicBlockCallback callback)
        {
            discovered[block.Index] = true;
            if (!callback(block, this))
                return;//Means stop
            foreach (var edge in block.PredecessorEdges)
            {
                if (!discovered[PredecessorEdges[edge].Index])
                {
                    DFSInverse(PredecessorEdges[edge], discovered, callback);
                }
            }
        }

        /// <summary>
        /// DFSInverse Depth First Search implementation using predecessors edge.
        /// </summary>
        /// <param name="terminalBlock">The terminal block.</param>
        /// <param name="callback">CallBack function</param>
        public void DFSInverse(BasicBlock<N, D> terminalBlock, BasicBlockCallback callback)
        {
            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            DFSInverse(terminalBlock, discovered, callback);
        }

        /// <summary>
        /// DFSInverse Depth First Search implementation using predecessors edge.
        /// </summary>
        /// <param name="callback">CallBack function</param>
        public void DFSInverse(BasicBlockCallback callback)
        {
            foreach (var terminal in TerminalsBlocks)
            {
                DFSInverse(terminal, callback);
            }
        }

        /// <summary>
        /// Iterative version of DFS Depth First Search implementation
        /// </summary>
        /// <param name="callback">CallBack function</param>
        public void DFSIterative(BasicBlockCallback callback)
        {
            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            foreach (var root in RootBlocks)
            {
                Stack<BasicBlock<N, D>> stack = new Stack<BasicBlock<N, D>>();
                stack.Push(root);
                while (stack.Count > 0)
                {
                    BasicBlock<N, D> block = stack.Pop();
                    if (!discovered[block.Index])
                    {
                        if (!callback(block, this))
                        {   //Don't traverse edges
                            continue;
                        }
                        foreach (var edge in block.SuccessorEdges)
                        {
                            stack.Push(SuccessorEdges[edge]);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Iterative version of DFS Depth First Search implementation using predecessors edge.
        /// </summary>
        /// <param name="callback">CallBack function</param>
        public void DFSIterativeInverse(BasicBlockCallback callback)
        {
            System.Collections.BitArray discovered = new System.Collections.BitArray(AllBlocks.Count);
            foreach (var root in TerminalsBlocks)
            {
                Stack<BasicBlock<N, D>> stack = new Stack<BasicBlock<N, D>>();
                stack.Push(root);
                while (stack.Count > 0)
                {
                    BasicBlock<N, D> block = stack.Pop();
                    if (!discovered[block.Index])
                    {
                        if (!callback(block, this))
                        {   //Don't traverse edges
                            continue;
                        }
                        foreach (var edge in block.PredecessorEdges)
                        {
                            stack.Push(PredecessorEdges[edge]);
                        }
                    }
                }
            }
        }
    }
}

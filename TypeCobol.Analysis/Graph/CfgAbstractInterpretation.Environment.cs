using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Util;

namespace TypeCobol.Analysis.Graph
{
    public partial class CfgAbstractInterpretation<N,D>
    {
        /// <summary>
        /// Abstract interpretation execution environment.
        /// The default execution performed is based on a dfs algorithm with an execution threshold on cyclic transition.
        /// This method only works on CFG graph emitted with an Extended or WithDfa mode.
        /// </summary>
        public class Environment
        {
            /// <summary>
            /// The Abstract Interpretation Stack
            /// </summary>
            private Stack<BasicBlock<N, D>> _interpretationStack;        

            /// <summary>
            /// The current execution stack
            /// </summary>
            public IReadOnlyCollection<BasicBlock<N, D>> ExecutionStack => _interpretationStack;

            /// <summary>
            /// The DFS Run stack
            /// </summary>
            private Stack<BasicBlock<N, D>> _dfsStack;
            /// <summary>
            /// Bit Vector to check if metric values have been taken in account for a block index.
            /// </summary>
            private System.Collections.BitArray _metricUpdated;
            /// <summary>
            /// Current Cyclic execution Threshold by block index.
            /// </summary>
            private Dictionary<int, int> _cyclicThreshold;
            /// <summary>
            /// The Cyclic transition from one block index to another block index
            /// </summary>
            private Dictionary<int, HashSet<int> >_cyclicTransition;
            /// <summary>
            /// Number maximal of cyclic execution for a block index.
            /// </summary>
            private int _cyclicExecutionThreshold ;
            /// <summary>
            /// Current CFG being run.
            /// </summary>
            public ControlFlowGraph<N, D> Cfg { get; private set; }
            private readonly IObserver[] _observers;
            /// <summary>
            /// Some metrics calculated.
            /// </summary>
            private Metrics _metrics;
            /// <summary>
            /// Constructeur
            /// </summary>
            /// <param name="observers">Array of execution observers if any, null otherwise</param>
            public Environment(params IObserver[] observers)
            {
                this._observers = observers;
            }

            /// <summary>
            /// Reset internal data structures.
            /// </summary>
            private void Reset()
            {
                if (_interpretationStack == null)
                    _interpretationStack = new Stack<BasicBlock<N, D>>();
                if (_dfsStack == null)
                    _dfsStack = new Stack<BasicBlock<N, D>>();
                _metricUpdated = new System.Collections.BitArray(Cfg.AllBlocks.Count);
                if (_cyclicThreshold == null)
                    _cyclicThreshold = new Dictionary<int, int>();
                else
                    _cyclicThreshold.Clear();
                if (_cyclicTransition == null)
                    _cyclicTransition = new Dictionary<int, HashSet<int>>();
                else
                    _cyclicTransition.Clear();
            }

            /// <summary>
            /// Run un abstract interpretation on a graph.
            /// </summary>
            /// <param name="cfg">The graph to be interpreted.</param>
            /// <param name="cyclicExecutionThreshold">The maximal count of cyclic execution for a cyclic block.</param>
            /// <returns>Execution metrics.</returns>
            public virtual Metrics Run(ControlFlowGraph<N, D> cfg, int cyclicExecutionThreshold = 0)
            {
                this.Cfg = cfg;
                this._cyclicExecutionThreshold = cyclicExecutionThreshold;
                Reset();
                SetCyclicityThreshold();
                this._metrics = new Metrics();                
                Run(cfg.RootBlock, null);
                System.Diagnostics.Debug.Assert(_interpretationStack.Count == 0);
                System.Diagnostics.Debug.Assert(_dfsStack.Count == 0);
                return this._metrics;
            }

            /// <summary>
            /// Set the Cyclicity Threshold for cyclic blocks of the graph.
            /// </summary>
            private void SetCyclicityThreshold()
            {
                // Domain : all blocks of the cfg graph.
                BitSet domain = new BitSet();
                Dictionary<int, int> dfsMark = new Dictionary<int, int>();
                Stack<int> dfsStack = new Stack<int>();

                //Compute the domain
                Cfg.DFS((block, incomingEdge, predecessorBlock, graph) => { domain.Set(block.Index); return true; });
                for (int blockIndex = domain.NextSetBit(0); blockIndex >= 0; blockIndex = domain.NextSetBit(blockIndex + 1))
                {
                    if (Visited(blockIndex) == 0)
                        dfs(blockIndex);
                }

                // Visit mark of a block 'a'
                // 0 : Block not visited
                // > 0 : Block in active consideration
                // Infinity : the cycle to which belongs the block has been considered.
                int Visited(int blockIndex)
                {
                    if (dfsMark.TryGetValue(blockIndex, out var blockIndexMark))
                        return blockIndexMark;
                    return 0;
                }

                // Travserse of all transitions from block 'a'
                void dfs(int blockIndex_a)
                {
                    dfsStack.Push(blockIndex_a);
                    int depth = dfsStack.Count;
                    dfsMark[blockIndex_a] = depth;
                    foreach (var s in Cfg.AllBlocks[blockIndex_a].SuccessorEdges)
                    {   // For each transition from block 'a' to block 'b'
                        var sb = Cfg.SuccessorEdges[s];
                        int blockIndex_b = sb.Index;
                        int mark_b = Visited(blockIndex_b);
                        if (mark_b == 0)
                        {   // Block 'b' is not visited yet => traverse it
                            dfs(blockIndex_b);
                            mark_b = Visited(blockIndex_b);
                        }
                        var mark_a = Visited(blockIndex_a);                        
                        if (mark_b < mark_a)
                        {   // This mean that the transition from block 'a' to block 'b' leads to a cycle.
                            _cyclicThreshold[blockIndex_b] = _cyclicExecutionThreshold;
                            if (!_cyclicTransition.TryGetValue(blockIndex_a, out var transitions))
                            {
                                transitions = new HashSet<int>();
                                _cyclicTransition.Add(blockIndex_a, transitions);
                            }
                            transitions.Add(blockIndex_b);
                        }
                    }
                    int mark = Visited(blockIndex_a);
                    if (mark == depth)

                    {
                        do
                        {   // Close any block that belongs to a cycle from block 'a' as considered.
                            dfsMark[dfsStack.Peek()] = Int32.MaxValue;
                        } while (dfsStack.Count > 0 && dfsStack.Pop() != blockIndex_a);
                    }
                }
            }

            /// <summary>
            /// Run a sub branch
            /// </summary>
            /// <param name="startBlock">The beginning branch</param>
            /// <param name="stopBlock">The stopping branch which will not be executed.</param>
            protected virtual void Run(BasicBlock<N, D> startBlock, BasicBlock<N, D> stopBlock)
            {
                _dfsStack.Push(startBlock);  
                while (_dfsStack.Count > 0)
                {
                    BasicBlock<N, D> block = _dfsStack.Pop();
                    if (block == stopBlock)
                        return;
                    if (CanExecute(block))
                    {
                        if (!_metricUpdated.Get(block.Index))
                        {
                            _metrics.NodeCount++;
                            _metrics.EdgeCount += block.SuccessorEdges.Count;     
                            if (block.Context != null) // This a control block.
                                _metrics.ControlSubgraphCount++;
                            _metricUpdated.Set(block.Index, true);
                        }
                        EnterBlock(block);
                        IterateBlock(block);
                        BasicBlock<N, D> nextFlowBlock = InterpretContext(block);                        
                        LeaveBlock(block);
                        if (nextFlowBlock == null)
                        {
                            foreach (var edge in block.SuccessorEdges)
                            {
                                var b = Cfg.SuccessorEdges[edge];
                                if (CanAddExecution(block, b))
                                {
                                    _dfsStack.Push(b);
                                }
                            }
                        }
                        else
                        {
                            if (CanAddExecution(block, nextFlowBlock))
                            {
                                _dfsStack.Push(nextFlowBlock);
                            }
                        }
                    }
                }

                /// <summary>
                /// CanExecute is used during execution to check for a block that contributes to a cycle,
                /// if can be executed, that is to say if its cyclic threshold is not consumed.
                /// Other blocks are always executed.
                /// </summary>
                /// <param name="b">The block to be checked</param>
                /// <returns>Return true if yes, false otherwise.</returns>
                bool CanExecute(BasicBlock<N, D> b)
                {
                    if (_cyclicThreshold.TryGetValue(b.Index, out int t))
                    {
                        return t >= 0;
                    }
                    return true;
                }
                /// <summary>
                /// Checks if the transition from one block to another can lead to a cycle.
                /// </summary>
                /// <param name="from">Souce block</param>
                /// <param name="to">Target block</param>
                /// <returns>Return true if yes, false otherwise.</returns>
                bool IsCyclic(BasicBlock<N, D> from, BasicBlock<N, D> to)
                {
                    if (_cyclicTransition.TryGetValue(from.Index, out var toindices))
                    {
                        return toindices.Contains(to.Index);
                    }
                    return false;
                }

                /// <summary>
                /// CanAddExecution Checks if a transition from one block to another can be pushed as to be executed.
                /// For a transition that can lead to a cycle, the cyclic threshold value is decreased, if the threshold
                /// is consumed the transition cannot be executed.
                /// </summary>
                /// <param name="from">Source block</param>
                /// <param name="to">Target block</param>
                /// <returns>True if the transition can be added, false otherwise.</returns>
                bool CanAddExecution(BasicBlock<N, D> from, BasicBlock<N, D> to)
                {
                    if (IsCyclic(from, to))
                    {
                        if (!_cyclicThreshold.TryGetValue(to.Index, out int t))
                        {
                            t = 0;
                        }
                        if (t >= 0 && t <= _cyclicExecutionThreshold)
                        {
                            _cyclicThreshold[to.Index] = t - 1;
                            return true;
                        }
                        return false;
                    }
                    return true;
                }
            }

            /// <summary>
            /// Iterate over all instructions in the given block.
            /// </summary>
            /// <param name="block"></param>
            private void IterateBlock(BasicBlock<N, D> block)
            {
                if (_observers != null && block.Instructions != null)
                {
                    foreach (var i in block.Instructions)
                    {
                        foreach (var observer in _observers)
                            observer.OnInstruction(i, block, this);
                    }
                }
            }

            /// <summary>
            /// Enter a block
            /// </summary>
            /// <param name="block"></param>
            private void EnterBlock(BasicBlock<N, D> block)
            {
                if (_observers != null)
                {
                    foreach (var observer in _observers)
                        observer.EnterBlock(block, this);
                }
            }

            /// <summary>
            /// Leave a block
            /// </summary>
            /// <param name="block"></param>
            public void LeaveBlock(BasicBlock<N, D> block)
            {
                if (_observers != null)
                {
                    foreach (var observer in _observers)
                        observer.LeaveBlock(block, this);
                }
            }

            /// <summary>
            /// Check if the given block has been relocated in the given context.
            /// </summary>
            /// <param name="ctx">The Context</param>
            /// <param name="fromBlock">The block to check</param>
            /// <returns>The relocated block if the block has been relocated, the same block otherwise</returns>
            private BasicBlock<N, D> CheckRelocatedBlock(MultiBranchContext<N, D> ctx, BasicBlock<N, D> fromBlock)
            {
                var relocIndex = ctx.GetRelocatedBlockIndex(fromBlock.Index);
                return relocIndex >= 0 ? Cfg.AllBlocks[relocIndex] : fromBlock;
            }

            /// <summary>
            /// Interpret a block associated with an execution context.
            /// </summary>
            /// <param name="block">THe block to interpret</param>
            /// <returns>The next block</returns>
            private BasicBlock<N, D> InterpretContext(BasicBlock<N, D> block)
            {
                // Track Branching blocks
                if (block.Context == null)
                {
                    return null;
                }

                _interpretationStack.Push(block);
                
                if (block.Context.SubContexts != null)
                {                    
                    //Instruction with sub context : run all sub context.
                    foreach (var ctx in block.Context.SubContexts)
                    {
                        Run(CheckRelocatedBlock(ctx, ctx.OriginBlock), CheckRelocatedBlock(ctx, ctx.NextFlowBlock));
                    }
                }
                else
                {
                    foreach (var b in block.Context.Branches)
                    {
                        Run(CheckRelocatedBlock(block.Context, b), CheckRelocatedBlock(block.Context, block.Context.NextFlowBlock));
                    }
                }

                System.Diagnostics.Debug.Assert(this._interpretationStack.Count > 0 &&
                                                this._interpretationStack.Peek() == block);
                this._interpretationStack.Pop();
                return CheckRelocatedBlock(block.Context, block.Context.NextFlowBlock);
            }
        }
    }
}

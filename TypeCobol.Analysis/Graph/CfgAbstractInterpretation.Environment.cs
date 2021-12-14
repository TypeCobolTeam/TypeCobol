using System.Collections.Generic;

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
            private readonly Stack<BasicBlock<N, D>> _interpretationStack;

            /// <summary>
            /// The current execution stack
            /// </summary>
            public IReadOnlyCollection<BasicBlock<N, D>> ExecutionStack => _interpretationStack;

            /// <summary>
            /// The DFS Run stack
            /// </summary>
            private readonly Stack<BasicBlock<N, D>> _dfsStack;

            /// <summary>
            /// Bit Vector to check if metric values have been taken in account for a block index.
            /// </summary>
            private System.Collections.BitArray _metricUpdated;

            /// <summary>
            /// Current Cyclic execution Threshold by block index.
            /// </summary>
            private readonly Dictionary<int, int> _cyclicThreshold;

            /// <summary>
            /// Number maximal of cyclic execution for a block index.
            /// </summary>
            private int _cyclicExecutionThreshold;

            /// <summary>
            /// Current CFG being run.
            /// </summary>
            public ControlFlowGraph<N, D> Cfg { get; private set; }

            /// <summary>
            /// Some metrics calculated.
            /// </summary>
            private Metrics _metrics;

            private readonly IObserver[] _observers;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="observers">Array of execution observers if any, null otherwise</param>
            public Environment(params IObserver[] observers)
            {
                _interpretationStack = new Stack<BasicBlock<N, D>>();
                _dfsStack = new Stack<BasicBlock<N, D>>();
                _cyclicThreshold = new Dictionary<int, int>();
                _observers = observers;
            }

            /// <summary>
            /// Reset internal data structures.
            /// </summary>
            private void Reset()
            {
                System.Diagnostics.Debug.Assert(_interpretationStack.Count == 0);
                System.Diagnostics.Debug.Assert(_dfsStack.Count == 0);
                _metricUpdated = new System.Collections.BitArray(Cfg.AllBlocks.Count);
                _cyclicThreshold.Clear();
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
                Dictionary<int, int> dfsMark = new Dictionary<int, int>();
                Stack<int> dfsStack = new Stack<int>();
                Traverse(Cfg.RootBlock.Index);

                // Visit mark of a block
                // 0 : Block not visited
                // > 0 : Block in active consideration
                // Infinity : the cycle to which belongs the block has been considered.
                int GetMark(int blockIndex) => dfsMark.TryGetValue(blockIndex, out var blockIndexMark) ? blockIndexMark : 0;

                // Traverse all transitions from current block
                void Traverse(int blockIndex)
                {
                    dfsStack.Push(blockIndex);
                    int depth = dfsStack.Count;
                    dfsMark[blockIndex] = depth;

                    int mark;
                    foreach (var successorEdge in Cfg.AllBlocks[blockIndex].SuccessorEdges)
                    {
                        // For each transition from current block to its successors
                        var successorBlock = Cfg.SuccessorEdges[successorEdge];
                        int successorBlockIndex = successorBlock.Index;
                        int successorMark = GetMark(successorBlockIndex);
                        if (successorMark == 0)
                        {
                            // Successor block has not been visited yet => traverse it and update its mark
                            Traverse(successorBlockIndex);
                            successorMark = GetMark(successorBlockIndex);
                        }

                        mark = GetMark(blockIndex);
                        if (successorMark < mark)
                        {
                            // This mean that the transition from current block to successorBlock leads to a cycle.
                            _cyclicThreshold[successorBlockIndex] = _cyclicExecutionThreshold;
                        }
                    }
                    
                    mark = GetMark(blockIndex);
                    if (mark == depth)
                    {
                        do
                        {
                            // Close any block that belongs to a cycle from current considered block.
                            dfsMark[dfsStack.Peek()] = int.MaxValue;
                        }
                        while (dfsStack.Count > 0 && dfsStack.Pop() != blockIndex);
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
                                _dfsStack.Push(b);
                            }
                        }
                        else
                        {
                            _dfsStack.Push(nextFlowBlock);
                        }
                    }
                }

                bool CanExecute(BasicBlock<N, D> b)
                {
                    if (_cyclicThreshold.TryGetValue(b.Index, out int t))
                    {
                        _cyclicThreshold[b.Index]--;//Block belongs to a cycle, consume 1 jump
                        return t >= 0;
                    }

                    return true;
                }
            }

            /// <summary>
            /// Determines if the given block is an anchor for cyclic execution.
            /// </summary>
            /// <param name="block">The block to be tested</param>
            /// <returns>true if yes, false otherwise</returns>
            public bool IsAnchorCyclic(BasicBlock<N, D> block)
            {
                return _cyclicThreshold.ContainsKey(block.Index);
            }

            /// <summary>
            /// Get the current cyclic execution threshold of anchor cyclic block.
            /// For an anchor block of a cyclic execution, this is the remaining cyclic execution threshold after entering the block. 
            /// For a block which is anchor of cyclic execution this value is >= 0 if it is still executable, 
            /// thus this value + 1 represents the cyclic threshold of the block before entering the block.            
            /// For a block which is not anchor of cyclic execution the return value is null.            
            /// </summary>
            /// <remarks>If needed, method IsAnchorCyclic(block) can be used to check if a block is cyclic anchor.</remarks>
            /// <param name="block">The block to get the current cyclic execution threshold value</param>
            /// <returns>The current cyclic execution threshold of an anchor cyclic block, null otherwise</returns>
            public int? GetCurrentCyclicExecutionThreshold(BasicBlock<N, D> block)
            {
                if (_cyclicThreshold.TryGetValue(block.Index, out int t))
                    return t;
                else
                    return null;
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
                var relocatedBlockIndex = ctx.GetRelocatedBlockIndex(fromBlock.Index);
                return relocatedBlockIndex >= 0 ? Cfg.AllBlocks[relocatedBlockIndex] : fromBlock;
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

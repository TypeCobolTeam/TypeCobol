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
        /// The default execution performed is based on a dfs algorithm, with exception that
        /// branching instructions are handled specifically.
        /// Thus not all execution paths are applied, but all nodes are visited using dfs like algorithm if they are accessible.
        /// A node cannot be visited twice.
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
            /// Current Cyclic execution Thresold by block index.
            /// </summary>
            private Dictionary<int,int> _cyclicThreshold;
            /// <summary>
            /// The Cyclic relation from one block index to another block index
            /// </summary>
            private Dictionary<int, HashSet<int> >_cyclicRelation;
            /// <summary>
            /// Number maximal of cyclic execution for a block index.
            /// </summary>
            private int _cyclicExecutionThresold ;
            /// <summary>
            /// Current CFG being run.
            /// </summary>
            public ControlFlowGraph<N, D> Cfg { get; private set; }
            private readonly IObserver[] _observers;
            /// <summary>
            /// Some metrics calculated.
            /// </summary>
            private Metrics _metrics;
            public Environment(params IObserver[] observers)
            {
                this._observers = observers;
            }

            /// <summary>
            /// Intialize internal data structures.
            /// </summary>
            private void Initialize()
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
                if (_cyclicRelation == null)
                    _cyclicRelation = new Dictionary<int, HashSet<int>>();
                else
                    _cyclicRelation.Clear();
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
                this._cyclicExecutionThresold = cyclicExecutionThreshold;
                Initialize();
                SetCyclicityThresold();
                this._metrics = new Metrics();                
                Run(cfg.RootBlock, null);
                System.Diagnostics.Debug.Assert(_interpretationStack.Count == 0);
                System.Diagnostics.Debug.Assert(_dfsStack.Count == 0);
                return this._metrics;
            }

            /// <summary>
            /// Set the Cyclicity Thresold for cyclic blocks of the graph.
            /// </summary>
            public void SetCyclicityThresold()
            {
                BitSet _domain = new BitSet();
                Dictionary<int, int> _N = new Dictionary<int, int>();
                Stack<int> _stack = new Stack<int>();

                //Compute the domain
                Cfg.DFS((block, incomingEdge, predecessorBlock, graph) => { _domain.Set(block.Index); return true; });
                for (int i = _domain.NextSetBit(0); i >= 0; i = _domain.NextSetBit(i + 1))
                {
                    if (Visited(i) == 0)
                        dfs(i);
                }

                int Visited(int a)
                {
                    if (_N.TryGetValue(a, out var na))
                        return na;
                    return 0;
                }
                void dfs(int a)
                {
                    _stack.Push(a);
                    int d = _stack.Count;
                    _N[a] = d;
                    foreach (var s in Cfg.AllBlocks[a].SuccessorEdges)
                    {
                        var sb = Cfg.SuccessorEdges[s];
                        int b = sb.Index;
                        int _nb = Visited(b);
                        if (_nb == 0)
                        {
                            dfs(b);
                        }
                        int _na = Visited(a);
                        _nb = Visited(b);
                        if (_nb < _na)
                        {
                            _cyclicThreshold[b] = _cyclicExecutionThresold;
                            if (!_cyclicRelation.TryGetValue(a, out var set))
                            {
                                set = new HashSet<int>();
                                _cyclicRelation.Add(a, set);
                            }
                            set.Add(b);
                        }
                    }
                    int na = Visited(a);
                    if (na == d)
                    {
                        while (_stack.Count > 0 && _stack.Pop() != a)
                        {
                        }
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

                bool CanExecute(BasicBlock<N, D> b)
                {
                    if (_cyclicThreshold.TryGetValue(b.Index, out int t))
                    {
                        return t >= 0;
                    }
                    return true;
                }
            }

            private bool IsCyclic(BasicBlock<N, D> from, BasicBlock<N, D> to)
            {
                if (_cyclicRelation.TryGetValue(from.Index, out var toindices))
                {
                    return toindices.Contains(to.Index);
                }
                return false;
            }

            private bool CanAddExecution(BasicBlock<N, D> from, BasicBlock<N, D> to)
            {
                if (IsCyclic(from, to))
                {
                    if (!_cyclicThreshold.TryGetValue(to.Index, out int t))
                    {
                        t = 0;
                    }
                    if (t >= 0 && t <= _cyclicExecutionThresold)
                    {
                        _cyclicThreshold[to.Index] = t - 1;
                        return true;
                    }
                    return false;
                }
                return true;
            }

            /// <summary>
            /// Iterate over all instruction in the given block.
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

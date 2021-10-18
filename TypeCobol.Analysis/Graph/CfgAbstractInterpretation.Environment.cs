using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    public partial class CfgAbstractInterpretation<N,D>
    {
        /// <summary>
        /// Abstract interpretation execution environment
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
            private Stack<BasicBlock<N, D>> _dFSStack;

            /// <summary>
            /// Already Visited Block
            /// </summary>
            private System.Collections.BitArray _visited;

            /// <summary>
            /// Current CFG being run.
            /// </summary>
            public ControlFlowGraph<N, D> Cfg { get; private set; }
            private IObserver[] _observers;
            /// <summary>
            /// Some metrics calculated.
            /// </summary>
            public Metrics Metrics { get; private set; }
            public Environment(params IObserver[] observers)
            {
                this._observers = observers;
            }

            public virtual Metrics Run(ControlFlowGraph<N, D> cfg)
            {
                this.Cfg = cfg;
                _interpretationStack = new Stack<BasicBlock<N, D>>();
                // We use an iterative DFS algorithm.
                _visited = new System.Collections.BitArray(Cfg.AllBlocks.Count);
                _dFSStack = new Stack<BasicBlock<N, D>>();
                this.Metrics = new Metrics();
                Run(Cfg.RootBlock, null);                
                return this.Metrics;
            }

            /// <summary>
            /// Run a sub branch
            /// </summary>
            /// <param name="startBlock">The beginning branch</param>
            /// <param name="stopBlock">The stopping branch which will not be executed.</param>
            protected virtual void Run(BasicBlock<N, D> startBlock, BasicBlock<N, D> stopBlock)
            {
                if (_visited.Get(startBlock.Index))
                    return;
                _dFSStack.Push(startBlock);
                while (_dFSStack.Count > 0)
                {
                    BasicBlock<N, D> block = _dFSStack.Pop();
                    if (block == stopBlock)
                        return;
                    if (!_visited[block.Index])
                    {
                        _visited.Set(block.Index, true);
                        Metrics.NodeCount++;
                        EnterBlock(block);
                        IterateBlock(block);
                        BasicBlock<N, D> nextFlowBlock = null;

                        // Track Branching blocks
                        if (block.Context != null)
                        {                            
                            nextFlowBlock = InterpretContext(block);
                        }
                        Metrics.EdgeCount += block.SuccessorEdges.Count;
                        LeaveBlock(block);                        
                        if (nextFlowBlock == null)
                        {
                            foreach (var edge in block.SuccessorEdges)
                            {
                                _dFSStack.Push(Cfg.SuccessorEdges[edge]);
                            }
                        }
                        else
                        {
                            _dFSStack.Push(nextFlowBlock);
                        }
                    }
                }
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
                return relocIndex >= 0 ? this.Cfg.AllBlocks[relocIndex] : fromBlock;
            }

            /// <summary>
            /// Interpret a block associated with an execution context.
            /// </summary>
            /// <param name="block">THe block to interpret</param>
            /// <returns>The next block</returns>
            private BasicBlock<N, D> InterpretContext(BasicBlock<N, D> block)
            {
                _interpretationStack.Push(block);

                Metrics.ControlSubgraphCount++;
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

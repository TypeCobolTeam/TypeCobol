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
            private Stack<BasicBlock<N, D>> InterpretationStack { get; set; }

            /// <summary>
            /// The current execution stack
            /// </summary>
            public IReadOnlyCollection<BasicBlock<N, D>> ExecutionStack => InterpretationStack;

            /// <summary>
            /// The DFS Run stack
            /// </summary>
            private Stack<BasicBlock<N, D>> DFSStack;

            /// <summary>
            /// Already Visited Block
            /// </summary>
            private System.Collections.BitArray Visited;

            /// <summary>
            /// Current CFG being run.
            /// </summary>
            public readonly ControlFlowGraph<N, D> Cfg;
            private IObserver[] _observers;
            /// <summary>
            /// Some metrics calculated.
            /// </summary>
            public readonly Metrics Metrics;
            public Environment(ControlFlowGraph<N, D> cfg, params IObserver[] observers)
            {
                this.Cfg = cfg;
                this._observers = observers;
                InterpretationStack = new Stack<BasicBlock<N, D>>();
                // We use an iterative DFS algorithm.
                Visited = new System.Collections.BitArray(cfg.AllBlocks.Count);
                DFSStack = new Stack<BasicBlock<N, D>>();
                this.Metrics = new Metrics();
            }

            public virtual void Run()
            {
                Run(Cfg.RootBlock, null);
            }

            /// <summary>
            /// Run a sub branch
            /// </summary>
            /// <param name="startBlock">The beginning branch</param>
            /// <param name="stopBlock">The stopping branch which will not be executed.</param>
            protected virtual void Run(BasicBlock<N, D> startBlock, BasicBlock<N, D> stopBlock)
            {
                if (Visited.Get(startBlock.Index))
                    return;
                DFSStack.Push(startBlock);
                while (DFSStack.Count > 0)
                {
                    BasicBlock<N, D> block = DFSStack.Pop();
                    if (block == stopBlock)
                        return;
                    if (!Visited[block.Index])
                    {
                        Visited.Set(block.Index, true);
                        Metrics.NodeCount++;
                        EnterBlock(block);
                        IterateBlock(block);
                        BasicBlock<N, D> nextFlowBlock = null;

                        // Track Branching blocks
                        if (block.Context != null)
                        {                            
                            nextFlowBlock = InterpretContext(block);
                        }

                        LeaveBlock(block);
                        if (nextFlowBlock == null)
                        {
                            foreach (var edge in block.SuccessorEdges)
                            {
                                DFSStack.Push(Cfg.SuccessorEdges[edge]);
                                Metrics.EdgeCount++;
                            }
                        }
                        else
                        {
                            DFSStack.Push(nextFlowBlock);                            
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
                if (_observers != null)
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
                InterpretationStack.Push(block);

                Metrics.ControlSubgraphCount++;
                if (block.Context.SubContexts != null)
                {                    
                    //Instruction with sub context : run all sub context.
                    foreach (var ctx in block.Context.SubContexts)
                    {
                        Metrics.EdgeCount++;
                        Run(CheckRelocatedBlock(ctx, ctx.OriginBlock), CheckRelocatedBlock(ctx, ctx.NextFlowBlock));
                    }
                }
                else
                {
                    foreach (var b in block.Context.Branches)
                    {
                        Metrics.EdgeCount++;
                        Run(CheckRelocatedBlock(block.Context, b), CheckRelocatedBlock(block.Context, block.Context.NextFlowBlock));
                    }
                    // Special case exemple IF THEN Without and ELSE => add virtual else to next block
                    Metrics.EdgeCount += block.Context.Branches.Count == 1 ? 1 : 0;
                }

                System.Diagnostics.Debug.Assert(this.InterpretationStack.Count > 0 &&
                                                this.InterpretationStack.Peek() == block);
                this.InterpretationStack.Pop();
                return CheckRelocatedBlock(block.Context, block.Context.NextFlowBlock);
            }

        }
    }
}

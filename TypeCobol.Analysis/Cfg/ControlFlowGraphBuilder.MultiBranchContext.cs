using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// The context of a Multi Branch instruction like IF or Evaluate.
        /// </summary>
        internal class MultiBranchContext : IMultiBranchContext<Node, D>
        {
            /// <summary>
            /// Origin block before multi branches
            /// </summary>
            internal BasicBlockForNode OriginBlock;
            /// <summary>
            /// List of multi branch blocks
            /// </summary>
            internal List<BasicBlock<Node, D>> Branches;
            /// <summary>
            /// Indices in the SuccessorEdges of all blocks in Branches
            /// </summary>
            internal List<int> BranchIndices;
            /// <summary>
            /// Root Block of the multi branches
            /// </summary>
            internal BasicBlockForNode RootBlock;
            /// <summary>
            /// The Index in the SuccessorEdge of the RootBlock if any, -1 otherwise.
            /// </summary>
            internal int RootBlockSuccessorIndex;
            /// <summary>
            /// Terminals block associated to Multi branch Context if any.
            /// Terminals blocks are block that have as successor NextFlowBlock or
            /// a branching to the beginning of a loop instruction block.
            /// </summary>
            public IList<BasicBlock<Node, D>> Terminals { get; internal set; }
            /// <summary>
            /// The instruction associated to this context.
            /// </summary>
            public Node Instruction { get; internal set; }
            /// <summary>
            /// List to hold When, WhenOther, WhenSearch and AtEnd nodes
            /// encountered in Evaluate or Search statements.
            /// </summary>
            internal List<Node> ConditionNodes;
            /// <summary>
            /// Next Flow Block calculated
            /// </summary>
            public BasicBlock<Node, D> NextFlowBlock { get; internal set; }
            /// <summary>
            /// Any Sub context if any, null otherwise.
            /// For instance in COBOL EVALUATE Context has all WHEN and WHENOTHER as sub contexts.
            /// </summary>
            public IList<IMultiBranchContext<Node, D>> SubContexts { get; internal set; }

            BasicBlock<Node, D> IMultiBranchContext<Node, D>.OriginBlock => this.OriginBlock;

            BasicBlock<Node, D> IMultiBranchContext<Node, D>.RootBlock => this.RootBlock;

            IList<BasicBlock<Node, D>> IMultiBranchContext<Node, D>.Branches => this.Branches;

            IList<int> IMultiBranchContext<Node, D>.BranchIndices => this.BranchIndices;

            /// <summary>
            /// Constructor
            /// </summary>            
            /// <param name="instruction">The instruction associated to this context </param>
            internal MultiBranchContext(Node instruction)
            {
                Branches = new List<BasicBlock<Node, D>>();
                BranchIndices = new List<int>();
                Instruction = instruction;
                RootBlockSuccessorIndex = -1;
            }

            /// <summary>
            /// Start multi branching
            /// </summary>
            /// <param name="originBlock"></param>
            internal void Start(BasicBlockForNode originBlock)
            {
                this.OriginBlock = originBlock;
            }

            /// <summary>
            /// End the multi branching.
            /// </summary>
            /// <param name="builder">The related CFG Builder</param>
            /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
            /// <param name="nextBlock">The next block for all branches</param>
            internal void End(ControlFlowGraphBuilder<D> builder, bool branchToNext, BasicBlockForNode nextBlock)
            {
                End(builder, branchToNext, OriginBlock, nextBlock);
            }
            /// <summary>
            /// End the multi branching.
            /// </summary>
            /// <param name="builder">The related CFG Builder</param>
            /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
            /// <param name="rootBlock">Root block of the multi branch</param>
            /// <param name="nextBlock">The next block for all branches</param>
            internal void End(ControlFlowGraphBuilder<D> builder, bool branchToNext, BasicBlockForNode rootBlock, BasicBlockForNode nextBlock)
            {
                System.Diagnostics.Debug.Assert(rootBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);

                Terminals = new List<BasicBlock<Node, D>>();
                //Add the next block to the successors.
                int nbIndex = builder.Cfg.SuccessorEdges.Count;
                builder.Cfg.SuccessorEdges.Add(nextBlock);
                //Link current block to all branches.
                foreach (var b in Branches)
                {
                    //Add branch to the successors
                    BranchIndices.Add(builder.Cfg.SuccessorEdges.Count);
                    rootBlock.SuccessorEdges.Add(builder.Cfg.SuccessorEdges.Count);
                    builder.Cfg.SuccessorEdges.Add(b);
                    //Next Block is a successor of the branch.
                    AddTerminalSuccessorEdge(builder, b, nbIndex);
                }
                if (branchToNext)
                {
                    rootBlock.SuccessorEdges.Add(nbIndex);
                    Terminals.Add(rootBlock);
                }
                NextFlowBlock = nextBlock;                
            }

            /// <summary>
            /// Add a block as branch
            /// </summary>
            /// <param name="block">The block to add</param>
            internal void AddBranch(BasicBlockForNode block)
            {
                this.Branches.Add(block);
            }

            /// <summary>
            /// Add to all terminal block, from a given block b, a given successor index.
            /// </summary>
            /// <param name="builder">The related CFG Builder</param>
            /// <param name="b">The starting block</param>
            /// <param name="nbIndex">The terminal successor index</param>
            /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
            internal void AddTerminalSuccessorEdge(ControlFlowGraphBuilder<D> builder, BasicBlock<Node, D> b, int nbIndex, HashSet<int> visitedBlockIndex = null)
            {
                if (visitedBlockIndex == null)
                {
                    visitedBlockIndex = new HashSet<int>();
                }
                if (visitedBlockIndex.Contains(b.Index))
                    return;
                visitedBlockIndex.Add(b.Index);

                if (b.SuccessorEdges.Count == 0)
                {
                    //Ending block has no successors.
                    if (!b.HasFlag(BasicBlock<Node, D>.Flags.Ending))
                    {
                        if (b != builder.Cfg.SuccessorEdges[nbIndex])
                        {//Don't create recursion to ourselves
                            b.SuccessorEdges.Add(nbIndex);
                            Terminals?.Add(b);
                        }
                    }
                }
                else
                {
                    foreach (var s in b.SuccessorEdges)
                    {
                        AddTerminalSuccessorEdge(builder, (BasicBlockForNode)builder.Cfg.SuccessorEdges[s], nbIndex, visitedBlockIndex);
                    }
                }
            }

            /// <summary>
            /// Get all terminal blocks from the given block.
            /// </summary>
            /// <param name="builder">The related CFG Builder</param>
            /// <param name="b">The starting block</param>
            /// <param name="accumulator">Accumulator of  terminal blocks</param>
            /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
            internal void GetTerminalSuccessorEdges(ControlFlowGraphBuilder<D> builder, BasicBlockForNode b, List<BasicBlock<Node, D>> accumulator, HashSet<int> visitedBlockIndex = null)
            {
                if (visitedBlockIndex == null)
                {
                    visitedBlockIndex = new HashSet<int>();
                }
                if (visitedBlockIndex.Contains(b.Index))
                    return;
                visitedBlockIndex.Add(b.Index);
                if (b.SuccessorEdges.Count == 0)
                {
                    accumulator.Add(b);
                }
                else foreach (var s in b.SuccessorEdges)
                {
                    GetTerminalSuccessorEdges(builder, (BasicBlockForNode)builder.Cfg.SuccessorEdges[s], accumulator, visitedBlockIndex);
                }
            }

        }
    }
}
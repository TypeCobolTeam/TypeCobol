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
        internal class MultiBranchContext
        {
            /// <summary>
            /// Current Block before multi branches
            /// </summary>
            internal BasicBlockForNode CurrentBlock;
            /// <summary>
            /// List of multi branch blocks
            /// </summary>
            internal List<BasicBlockForNode> Branches;
            /// <summary>
            /// The Target Control Flow Graph Builder
            /// </summary>
            internal ControlFlowGraphBuilder<D> Builder;
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
            /// The instruction associated to this context.
            /// </summary>
            internal Node Instruction;

            /// <summary>
            /// Any contextual Data.
            /// </summary>
            internal object ContextualData;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="currentProgramCfgBuilder">The related CFG Builder</param>
            /// <param name="instruction">The instruction associated to this context </param>
            internal MultiBranchContext(ControlFlowGraphBuilder<D> currentProgramCfgBuilder, Node instruction)
            {
                Branches = new List<BasicBlockForNode>();
                BranchIndices = new List<int>();
                Builder = currentProgramCfgBuilder;
                Instruction = instruction;
                RootBlockSuccessorIndex = -1;
            }

            /// <summary>
            /// Start multi branching
            /// </summary>
            /// <param name="CurrentBlock"></param>
            internal void Start(BasicBlockForNode currentBlock)
            {
                this.CurrentBlock = currentBlock;
            }

            /// <summary>
            /// End the multi branching.
            /// </summary>
            /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
            /// <param name="nextBlock">The next block for all branches</param>
            internal void End(bool branchToNext, BasicBlockForNode nextBlock)
            {
                End(branchToNext, CurrentBlock, nextBlock);
            }
            /// <summary>
            /// End the multi branching.
            /// </summary>
            /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
            /// <param name="rootBlock">Root block of the multi branch</param>
            /// <param name="nextBlock">The next block for all branches</param>
            internal void End(bool branchToNext, BasicBlockForNode rootBlock, BasicBlockForNode nextBlock)
            {
                System.Diagnostics.Debug.Assert(Builder != null);
                System.Diagnostics.Debug.Assert(rootBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);
                //Add the next block to the successors.
                int nbIndex = Builder.Cfg.SuccessorEdges.Count;
                Builder.Cfg.SuccessorEdges.Add(nextBlock);
                //Link current block to all branches.
                foreach (var b in Branches)
                {
                    //Add branch to the successors
                    BranchIndices.Add(Builder.Cfg.SuccessorEdges.Count);
                    rootBlock.SuccessorEdges.Add(Builder.Cfg.SuccessorEdges.Count);
                    Builder.Cfg.SuccessorEdges.Add(b);
                    //Next Block is a successor of the branch.
                    AddTerminalSuccessorEdge(b, nbIndex);
                }
                if (branchToNext)
                {
                    rootBlock.SuccessorEdges.Add(nbIndex);
                }
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
            /// <param name="b">The starting block</param>
            /// <param name="nbIndex">The terminal successor index</param>
            /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
            internal void AddTerminalSuccessorEdge(BasicBlockForNode b, int nbIndex, HashSet<int> visitedBlockIndex = null)
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
                        if (b != Builder.Cfg.SuccessorEdges[nbIndex])
                        {//Don't create recursion to ourselves
                            b.SuccessorEdges.Add(nbIndex);
                        }
                    }
                }
                else
                {
                    foreach (var s in b.SuccessorEdges)
                    {
                        AddTerminalSuccessorEdge((BasicBlockForNode)Builder.Cfg.SuccessorEdges[s], nbIndex, visitedBlockIndex);
                    }
                }
            }

            /// <summary>
            /// Get all terminal blocks from the given block.
            /// </summary>
            /// <param name="b">The starting block</param>
            /// <param name="accumulator">Accumulator of  terminal blocks</param>
            /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
            internal void GetTerminalSuccessorEdges(BasicBlockForNode b, List<BasicBlockForNode> accumulator, HashSet<int> visitedBlockIndex = null)
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
                    GetTerminalSuccessorEdges((BasicBlockForNode)Builder.Cfg.SuccessorEdges[s], accumulator, visitedBlockIndex);
                }
            }

        }
    }
}
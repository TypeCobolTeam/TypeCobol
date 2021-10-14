using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// The context of a Multi Branch instruction like IF or Evaluate.
    /// </summary>
    internal class MultiBranchContext<N, D> : System.ICloneable
    {
        /// <summary>
        /// Origin block before multi branches
        /// </summary>
        internal BasicBlock<N, D> OriginBlock;
        /// <summary>
        /// List of multi branch blocks
        /// </summary>
        public IList<BasicBlock<N, D>> Branches { get; internal set; }
        /// <summary>
        /// Indices in the SuccessorEdges of all blocks in Branches
        /// </summary>
        public IList<int> BranchIndices { get; internal set; }
        /// <summary>
        /// Root Block of the multi branches
        /// </summary>
        internal BasicBlock<N, D> RootBlock;
        /// <summary>
        /// The Index in the SuccessorEdge of the RootBlock if any, -1 otherwise.
        /// </summary>
        internal int RootBlockSuccessorIndex;
        /// <summary>
        /// Terminals block associated to Multi branch Context if any.
        /// Terminals blocks are block that have as successor NextFlowBlock or
        /// a branching to the beginning of a loop instruction block.
        /// </summary>
        public IList<BasicBlock<N, D>> Terminals { get; internal set; }
        /// <summary>
        /// The instruction associated to this context.
        /// </summary>
        public N Instruction { get; internal set; }
        /// <summary>
        /// List to hold When, WhenOther, WhenSearch and AtEnd nodes
        /// encountered in Evaluate or Search statements.
        /// </summary>
        internal List<N> ConditionNodes;
        /// <summary>
        /// Block of the next flow
        /// </summary>
        public BasicBlock<N, D> NextFlowBlock { get; internal set; }
        /// <summary>
        /// Any Sub context if any, null otherwise.
        /// For instance in COBOL EVALUATE Context has all WHEN and WHENOTHER as sub contexts.
        /// </summary>
        public IList<MultiBranchContext<N, D>> SubContexts { get; internal set; }

        private BasicBlock<N, D> RootBlockForEnd;
        /// <summary>
        /// The Cloned Relocated BlockNode Map: Map Original Block Index to their Cloned and relocated Block Index, for instance block of
        /// target Paragraph cloned and relocated for a PERFORM instruction target.
        /// </summary>
        internal IDictionary<int, int> ClonedRelocatedMap { get; set; }

        /// <summary>
        /// Constructor
        /// </summary>            
        /// <param name="instruction">The instruction associated to this context </param>
        internal MultiBranchContext(N instruction)
        {
            Branches = new List<BasicBlock<N, D>>();
            BranchIndices = new List<int>();
            Instruction = instruction;
            RootBlockSuccessorIndex = -1;
        }

        /// <summary>
        /// Start multi branching
        /// </summary>
        /// <param name="originBlock"></param>
        internal void Start(BasicBlock<N, D> originBlock)
        {
            this.OriginBlock = originBlock;
            this.OriginBlock.Context = this;
        }

        /// <summary>
        /// End the multi branching.
        /// </summary>
        /// <param name="cfg">The related CFG Graph</param>
        /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
        /// <param name="nextBlock">The next block for all branches</param>
        internal void End(ControlFlowGraph<N, D> cfg, bool branchToNext, BasicBlock<N, D> nextBlock)
        {
            End(cfg, branchToNext, OriginBlock, nextBlock);
        }
        /// <summary>
        /// End the multi branching.
        /// </summary>
        /// <param name="cfg">The related CFG Graph</param>
        /// <param name="branchToNext">True if the CurrentBlock must be linked to the next branch also, false otherwise</param>
        /// <param name="rootBlock">Root block of the multi branch</param>
        /// <param name="nextBlock">The next block for all branches</param>
        internal void End(ControlFlowGraph<N, D> cfg, bool branchToNext, BasicBlock<N, D> rootBlock, BasicBlock<N, D> nextBlock)
        {
            System.Diagnostics.Debug.Assert(rootBlock != null);
            System.Diagnostics.Debug.Assert(nextBlock != null);

            Terminals = new List<BasicBlock<N, D>>();
            //Add the next block to the successors.
            int nbIndex = cfg.SuccessorEdges.Count;
            cfg.SuccessorEdges.Add(nextBlock);
            //Link current block to all branches.
            foreach (var b in Branches)
            {
                //Add branch to the successors
                BranchIndices.Add(cfg.SuccessorEdges.Count);
                rootBlock.SuccessorEdges.Add(cfg.SuccessorEdges.Count);
                cfg.SuccessorEdges.Add(b);
                //Next Block is a successor of the branch.
                AddTerminalSuccessorEdge(cfg, b, nbIndex);
            }
            if (branchToNext)
            {
                rootBlock.SuccessorEdges.Add(nbIndex);
                Terminals.Add(rootBlock);
            }
            RootBlockForEnd = rootBlock;
            NextFlowBlock = nextBlock;
        }

        /// <summary>
        /// Change the successors
        /// </summary>
        /// <param name="cfg">The target CFG graph</param>
        /// <param name="currentSucc">The current Successor</param>
        /// <param name="newSucc">The new Successor</param>
        internal void ChangeSuccessor(ControlFlowGraph<N, D> cfg, BasicBlock<N, D> currentSucc, BasicBlock<N, D> newSucc)
        {
            if (NextFlowBlock == currentSucc)
                NextFlowBlock = newSucc;
            if (RootBlockForEnd != null)
            {
                List<BasicBlock<N, D>> branches = (List<BasicBlock<N, D>>)Branches;
                List<int> branchIndices = (List<int>)BranchIndices;
                System.Diagnostics.Debug.Assert(branches.Count == branchIndices.Count);
                for (int i = 0; i < branches.Count; i++)
                {
                    if (branches[i] == currentSucc)
                    {
                        System.Diagnostics.Debug.Assert(cfg.SuccessorEdges[branchIndices[i]] == currentSucc);
                        branches[i] = newSucc;
                        RootBlockForEnd.SuccessorEdges.Remove(branchIndices[i]);
                        branchIndices[i] = cfg.SuccessorEdges.Count;
                        RootBlockForEnd.SuccessorEdges.Add(cfg.SuccessorEdges.Count);
                        cfg.SuccessorEdges.Add(newSucc);
                        break;
                    }
                }
            }
            if (Terminals != null)
            {
                List<BasicBlock<N, D>> terminals = (List<BasicBlock<N, D>>)Terminals;
                for (int i = 0; i < terminals.Count; i++)
                {
                    if (terminals[i] == currentSucc)
                    {
                        terminals[i] = newSucc;
                        break;
                    }
                }
            }
        }

        /// <summary>
        /// Add a block as branch
        /// </summary>
        /// <param name="block">The block to add</param>
        internal void AddBranch(BasicBlock<N, D> block)
        {
            this.Branches.Add(block);
        }

        /// <summary>
        /// Add to all terminal block, from a given block b, a given successor index.
        /// </summary>
        /// <param name="cfg">The related CFG Graph</param>
        /// <param name="b">The starting block</param>
        /// <param name="nbIndex">The terminal successor index</param>
        /// <param name="visitedBlockIndex">Set of already visited Block Index</param>
        internal void AddTerminalSuccessorEdge(ControlFlowGraph<N, D> cfg, BasicBlock<N, D> b, int nbIndex, HashSet<int> visitedBlockIndex = null)
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
                if (!b.HasFlag(BasicBlock<N, D>.Flags.Ending))
                {
                    if (b != cfg.SuccessorEdges[nbIndex])
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
                    AddTerminalSuccessorEdge(cfg, (BasicBlock<N, D>)cfg.SuccessorEdges[s], nbIndex, visitedBlockIndex);
                }
            }
        }

        /// <summary>
        /// Get the relocated block index created from the given initial block index.
        /// A relocated block index is the block index of a duplicated block node, where the initial block node was part
        /// of a BasicBlockForNode corresponding to a target paragraph or section of a PERFORM instruction.
        /// </summary>
        /// <param name="initialIndex">The initial Block Index</param>
        /// <returns>The block index after relocation if any -1 otherwise</returns>
        internal int GetRelocatedBlockIndex(int initialIndex)
        {
            if (this.ClonedRelocatedMap != null)
            {
                if (this.ClonedRelocatedMap.TryGetValue(initialIndex, out var duplicateIndex))
                {
                    return duplicateIndex;
                }
            }
            return -1;
        }

        public object Clone()
        {
            return base.MemberwiseClone();
        }

    }
}

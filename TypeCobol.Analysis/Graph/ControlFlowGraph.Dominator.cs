using System;
using System.Collections.Generic;
using TypeCobol.Analysis.Util;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// Dominator algorithm implementation
    /// </summary>
    /// <typeparam name="N"></typeparam>
    /// <typeparam name="D"></typeparam>
    public partial class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// Compute a Dominator set on this CFG Graph
        /// </summary>
        /// <returns>A Tuple of a List of reachable nodes and an array of BitSet indexed by BasicBlock Indices</returns>
        public Tuple<List<int>,BitSet[]> ComputeDominators()
        {
            BitSet[] dominators = new BitSet[this.AllBlocks.Count];
            //Compute reachable blocks
            List<int> blocks = new List<int>();
            this.DFS((block, incomingEdge, predecessorBlock, cfg) =>
            {
                blocks.Add(block.Index);
                dominators[block.Index] = new BitSet(dominators.Length);
                if (block.Index != this.RootBlock.Index)
                    dominators[block.Index].Full();
                return true;
            });
            //Setup prececessors
            this.SetupPredecessorEdgesFromRoot();

            //Reflexivity on root block
            dominators[this.RootBlock.Index].Set(this.RootBlock.Index);

            bool change = false;
            BitSet workSet = new BitSet(dominators.Length); //workingSet
            do
            {
                change = false;
                foreach (int n in blocks)
                {
                    if (n == this.RootBlock.Index)
                        continue;
                    BasicBlock<N, D> b = this.AllBlocks[n];
                    System.Diagnostics.Debug.Assert(b.PredecessorEdges != null);
                    workSet.Full();
                    foreach (int r in b.PredecessorEdges)
                    {
                        BasicBlock<N, D> p = PredecessorEdges[r];
                        workSet.And(dominators[p.Index]);
                    }
                    workSet.Set(n);//reflexivity
                    if (!workSet.Equals(dominators[n]))
                    {
                        dominators[n].Copy(workSet);
                        change = true;
                    }
                }
            } while (change);

            return new Tuple<List<int>, BitSet[]>(blocks, dominators);
        }

        /// <summary>
        /// Compute the immediate dominance relation.
        /// </summary>
        /// <param name="blocks">The blocks of the graph on which to work.</param>
        /// <param name="dominators">The dominator sets</param>
        /// <param name="duplicate">true to create new sets, rather then pruning dominators set</param>
        /// <returns>The immediate dominance sets</returns>
        public BitSet[] ComputeImmediateDominators(List<int> blocks, BitSet[] dominators, bool duplicate = true)
        {
            BitSet[] idoms = duplicate ? new BitSet[dominators.Length] : dominators;
            foreach (var b in blocks)
            {   
                if (duplicate)
                {
                    idoms[b] = new BitSet(dominators.Length);
                    idoms[b].Copy(dominators[b]);
                }
                idoms[b].Clear(b); //Remove reflexivity
            }
            BitSet tmp = new BitSet();
            foreach (var r in blocks)
            {
                if (r == this.RootBlock.Index) continue;
                tmp.Copy(idoms[r]);
                foreach (var s in blocks)
                {
                    if (tmp.Get(s))
                    {
                        foreach (var t in blocks)
                        {
                            if (t == s) continue;
                            if (tmp.Get(t) && idoms[s].Get(t))
                            {
                                idoms[r].Clear(t);
                            }
                        }
                    }
                }
            }
            return idoms;
        }

        /// <summary>
        /// Dump a set of Dominators
        /// </summary>
        /// <param name="dominators">The set of Dominators</param>
        /// <param name="writer"></param>
        public static void DumpDominators(BitSet[] dominators, System.IO.TextWriter writer)
        {
            for (int i = 0; i < dominators.Length; i++)
            {
                BitSet doms = dominators[i];
                if (doms != null)
                {
                    writer.WriteLine($"Dominators[Block{i}]={doms.ToString()}");
                }
            }
        }
    }
}

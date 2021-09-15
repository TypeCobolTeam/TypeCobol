using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// CFG Transformer class for handling Iterative PERFORM PROCEDURE with an AFTER clause.
        /// </summary>
        private class CfgAfterIterativePerformProcedureTransformer : ICfgTransform<Node, D>
        {
            private HashSet<BasicBlockForNodeGroup> _visitedGroups;
            private ControlFlowGraph<Node, D> _cfg;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="builder"></param>
            public CfgAfterIterativePerformProcedureTransformer(ControlFlowGraph<Node, D> cfg)
            {
                this._cfg = cfg;
            }
            private bool Callback(BasicBlock<Node, D> block, int incomingEdge, BasicBlock<Node, D> predecessorBlock, ControlFlowGraph<Node, D> cfg)
            {
                //Search for an iterative group (with after) among successors of the current block
                int removedIndex = -1;
                BasicBlockForNodeGroup iterativeGroup = null;
                for (int i = 0; i < block.SuccessorEdges.Count; i++)
                {
                    int edge = block.SuccessorEdges[i];
                    var successor = cfg.SuccessorEdges[edge];
                    if (successor is BasicBlockForNodeGroup successorGroup)
                    {
                        if (successorGroup.IsIterativeGroup && edge != successorGroup.EntryIndexInSuccessors && successorGroup.Group.Count > 0)
                        {
                            if (successorGroup.IsAfterIterativeGroup)
                            {
                                iterativeGroup = successorGroup;
                                removedIndex = i;
                            }
                            break;
                        }
                    }
                }

                //If an after iterative group has been found, we must break the edge pointing to the PERFORM
                //and replace it with a new edge pointing to the first instruction of the group.
                if (iterativeGroup != null)
                {
                    //Remove incoming edge
                    block.SuccessorEdges.RemoveAt(removedIndex);

                    //Create an edge to the first block of the iterative group.
                    int entranceEdge = cfg.SuccessorEdges.Count;
                    cfg.SuccessorEdges.Add(iterativeGroup.Group.First.Value);
                    block.SuccessorEdges.Add(entranceEdge);
                    System.Diagnostics.Debug.Assert(block.Context == null);
                    //If the assert above fails (it should not be case), then it will be necessay to exeute the commented code bellow.
                    //if(block.Context != null)
                    //{
                    //    ((MultiBranchContext)block.Context).ChangeSuccessor(_cfg, iterativeGroup, iterativeGroup.Group.First.Value);
                    //}
                }

                //If the current block is a group, we must also traverse blocks of the group
                if (block is BasicBlockForNodeGroup group)
                {
                    if (group.Group.Count > 0)
                    {
                        if (_visitedGroups.Add(group))
                        {
                            cfg.DFS(group.Group.First.Value, Callback);
                        }
                    }
                }

                return true;
            }

            public ControlFlowGraph<Node, D> Transform(ControlFlowGraph<Node, D> graph)
            {
                _visitedGroups = new HashSet<BasicBlockForNodeGroup>();
                graph.DFS(Callback);
                _visitedGroups = null;
                return graph;
            }
        }
    }
}

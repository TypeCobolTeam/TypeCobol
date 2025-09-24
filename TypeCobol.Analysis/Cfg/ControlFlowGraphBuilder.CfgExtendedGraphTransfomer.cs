using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// CFG Transformer class for transforming a standard graph in to an extended graph.
        /// This is done by computing fixed point, until all Groups have been grafted to the continuation graph.
        /// </summary>
        private class CfgExtendedGraphTransfomer : ICfgTransform<Node, D>
        {
            public CfgExtendedGraphTransfomer(ControlFlowGraphBuilder<D> builder)
            {
                _builder = builder;
            }
            ControlFlowGraphBuilder<D> _builder;
            Stack<ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup> _parents;
            int _ChangeCount = 0;
            ControlFlowGraph<Node, D> _cfg;
            /// <summary>
            /// The relation that tells if a BasicBlockForNodeGroup is a parent of another group.
            /// </summary>
            Dictionary<ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup, bool> _IsParent;
            private bool GroupParenter(BasicBlock<Node, D> block, int incomingEdge, BasicBlock<Node, D> predecessorBlock, ControlFlowGraph<Node, D> cfg)
            {
                if (block is ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup group)
                {
                    if (!group.HasFlag(BasicBlock<Node, D>.Flags.GroupGrafted) && !_IsParent.ContainsKey(group))
                    {//ThisGroup is not already grafted treat it
                        _ChangeCount++;
                        if (_parents.Count > 0)
                        {
                            var p = _parents.Peek();
                            _IsParent[p] = true;
                        }
                        _parents.Push(group);
                        _IsParent[group] = false;
                        BasicBlock<Node, D> first = group.Group.First.Value;
                        _cfg.DFS(first, GroupParenter);
                        _parents.Pop();
                    }
                }
                return true;
            }
            public ControlFlowGraph<Node, D> Transform(ControlFlowGraph<Node, D> graph)
            {
                _cfg = graph;
                _parents = new Stack<ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup>();
                _IsParent = new Dictionary<ControlFlowGraphBuilder<D>.BasicBlockForNodeGroup, bool>();
                do
                {
                    _ChangeCount = 0;
                    _parents.Clear();
                    _IsParent.Clear();
                    //Firts we must determine those remaining groups that are not inclued in another group.
                    _cfg.DFS(GroupParenter);
                    //Now Graft all groups that are not parent
                    foreach (var e in _IsParent)
                    {
                        if (!e.Value)
                        {//It is a single group ==> Graft it.
                            var group = e.Key;
                            _builder.ExtendGroup(group, true);
                        }
                    }
                } while (_ChangeCount > 0);
                return graph;
            }
        }
    }
}

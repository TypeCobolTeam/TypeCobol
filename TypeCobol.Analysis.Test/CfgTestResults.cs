using System;
using System.Collections.Generic;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis.Util;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// Represent results of CFG building.
    /// </summary>
    /// <typeparam name="D"></typeparam>
    internal class CfgTestResults<D>
    {
        /// <summary>
        /// All graphs built for the initial source code.
        /// </summary>
        public IList<ControlFlowGraph<Node, D>> Graphs { get; }

        public CfgTestResults(IList<ControlFlowGraph<Node, D>> graphs)
        {
            Graphs = graphs;
        }
    }

    /// <summary>
    /// Represent results of CFg + DFA analysis.
    /// </summary>
    internal class DfaTestResults : CfgTestResults<DfaBasicBlockInfo<VariableSymbol>>
    {
        private class DfaData
        {
            public List<DfaUsePoint<Node, VariableSymbol>> UseList { get; }

            public List<DfaDefPoint<Node, VariableSymbol>> DefList { get; }

            public Dictionary<VariableSymbol, BitSet> VariableDefMap { get; }

            public DfaData(DataFlowGraphBuilder<Node, VariableSymbol> builder)
            {
                UseList = builder.UseList;
                DefList = builder.DefList;
                VariableDefMap = builder.VariableDefMap;
            }
        }

        private readonly Dictionary<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>, DfaData> _dfaData;

        public DfaTestResults(IList<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>> graphs)
            : base(graphs)
        {
            _dfaData = new Dictionary<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>, DfaData>();
            foreach (var graph in graphs)
            {
                var builder = new DefaultDataFlowGraphBuilder(graph);
                builder.ComputeUseDefSet();
                _dfaData.Add(graph, new DfaData(builder));
            }
        }

        private T Get<T>(ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>> graph, Func<DfaData, T> selector)
        {
            if (_dfaData.TryGetValue(graph, out var data))
            {
                return selector(data);
            }
            throw new ArgumentException("Supplied CFG has no associated DFA data.");
        }

        /// <summary>
        /// Return DFA UseList of a given graph.
        /// </summary>
        /// <param name="graph">Selected graph, it must belong to the Graphs collection of this instance.</param>
        /// <returns>The list of use points.</returns>
        public List<DfaUsePoint<Node, VariableSymbol>> GetUseList(ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>> graph)
            => Get(graph, data => data.UseList);

        /// <summary>
        /// Return DFA DefList of a given graph.
        /// </summary>
        /// <param name="graph">Selected graph, it must belong to the Graphs collection of this instance.</param>
        /// <returns>The list of def points.</returns>
        public List<DfaDefPoint<Node, VariableSymbol>> GetDefList(ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>> graph)
            => Get(graph, data => data.DefList);

        /// <summary>
        /// Return the DFA VariableDefMap of a given graph.
        /// </summary>
        /// <param name="graph">Selected graph, it must belong to the Graphs collection of this instance.</param>
        /// <returns>The dictionary of DefSets for each known variable.</returns>
        public Dictionary<VariableSymbol, BitSet> GetVariableDefMap(ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>> graph)
            => Get(graph, data => data.VariableDefMap);
    }
}

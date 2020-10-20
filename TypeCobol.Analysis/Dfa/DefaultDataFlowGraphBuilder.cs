using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Data Flow Graph Builder for Data Flow Analysis, a Cobol E-I Specialization.
    /// </summary>
    public class DefaultDataFlowGraphBuilder : DataFlowGraphBuilder<Node, VariableSymbol>
    {
        private static HashSet<VariableSymbol> GetSymbols(Dictionary<StorageArea, VariableSymbol> symbolDictionary)
        {
            var result = new HashSet<VariableSymbol>();
            if (symbolDictionary != null)
            {
                foreach (var p in symbolDictionary)
                {
                    result.Add(p.Value);
                }
            }

            return result;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg"></param>
        public DefaultDataFlowGraphBuilder(ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>> cfg)
            : base(cfg)
        {

        }

        /// <summary>
        /// Get Use Variables for a given node.
        /// </summary>
        /// <param name="node">The node</param>
        /// <returns>The set of used variables</returns>
        public override HashSet<VariableSymbol> GetUseVariables(Node node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            return GetSymbols(node.StorageAreaReadsSymbol);
        }

        /// <summary>
        /// Get Defined Variables for a given node
        /// </summary>
        /// <param name="node">The node</param>
        /// <returns>The set of defined variables</returns>
        public override HashSet<VariableSymbol> GetDefVariables(Node node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            return GetSymbols(node.StorageAreaWritesSymbol);
        }
    }
}

using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;
using System.Linq;

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

        protected override void CollectDataDefinitions()
        {
            if (Cfg != null && Cfg.IsInitialized)
            {
                var root = Cfg.RootBlock;
                var dataDivision = this.Cfg.ProcedureDivisionNode.Parent.GetChildren<DataDivision>().FirstOrDefault();
                if (dataDivision != null)
                {
                    foreach(var dataDef in Collect(dataDivision))
                    {
                        root.Instructions.AddFirst(dataDef);
                    }
                }
                IEnumerable<DataDefinition> Collect(Node node)
                {
                    if (node.SemanticData is VariableSymbol variable && variable.Value != null)
                    {
                        yield return node as DataDefinition;
                    }

                    if (node.ChildrenCount > 0)
                    {
                        foreach (var child in node.Children)
                        {
                            foreach (var childVariable in Collect(child))
                            {
                                yield return childVariable;
                            }
                        }
                    }
                }
            }
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
            return node is DataDefinition ? new HashSet<VariableSymbol>() { node.SemanticData as VariableSymbol} : GetSymbols(node.StorageAreaWritesSymbol);
        }
    }
}

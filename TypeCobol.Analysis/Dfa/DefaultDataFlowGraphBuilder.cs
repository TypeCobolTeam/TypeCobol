using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Data Flow Graph Builder for Data Flow Analysis, a Cobol E-I Specialization.
    /// </summary>
    public class DefaultDataFlowGraphBuilder : DataFlowGraphBuilder<Node, VariableSymbol>
    {
        /// <summary>
        /// Collector of DataDefinition Nodes in the list of Instructions of a given block.
        /// </summary>
        public class DataDefinitionCollector : AbstractSymbolAndTypeVisitor<BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>> , BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>> >
        {
            public override BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>>  VisitSymbol(Symbol s, BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>>  block)
            {
                s.Type?.Accept(this, block);
                return block;
            }

            public override BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>>  VisitType(Type t, BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>> block)
            {
                t.TypeComponent?.Accept(this, block);
                return block;
            }

            public override BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>>  VisitVariableSymbol(VariableSymbol s, BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>> block)
            {
                if (s.Value != null)
                    block.Instructions.AddFirst(s.TargetNode as DataDefinition);
                return VisitSymbol(s, block);
            }

            public override BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>>  VisitGroupType(Compiler.Types.GroupType t, BasicBlock<Node, DfaBasicBlockInfo<VariableSymbol>> block)
            {
                foreach (var field in t.Fields)
                {
                    field.Accept(this, block);
                }
                return block;
            }
        }

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
                var prg = Cfg.ProgramOrFunctionNode;
                var prgSymbol = (ProgramSymbol)prg.SemanticData;
                DataDefinitionCollector ddc = new DataDefinitionCollector();
                foreach(var vs in prgSymbol.WorkingStorageData)
                {
                    vs.Accept(ddc, root);
                }
                foreach (var vs in prgSymbol.LocalStorageData)
                {
                    vs.Accept(ddc, root);
                }
                foreach (var vs in prgSymbol.LinkageData)
                {
                    vs.Accept(ddc, root);
                }
                foreach (var vs in prgSymbol.FileData)
                {
                    vs.Accept(ddc, root);
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

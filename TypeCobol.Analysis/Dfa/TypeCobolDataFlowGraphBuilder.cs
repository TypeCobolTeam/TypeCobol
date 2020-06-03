using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Data Flow Graph Builder for Data Flow Analysis, a TypeCobol Specialization.
    /// </summary>
    public class TypeCobolDataFlowGraphBuilder : DataFlowGraphBuilder<Node, Symbol>
    {
        /// <summary>
        /// Vistor on StorageArea to collect them.
        /// </summary>
        internal class StorageAreaVisitor : TypeCobol.Compiler.CodeElements.AbstractAstVisitor
        {
            /// <summary>
            /// Kind of Storage Area Requested
            /// </summary>
            [Flags]
            internal enum Kind : byte
            {
                Read = 0x01 << 0,
                Write = 0x01 << 1
            }

            /// <summary>
            /// All visited symbols.
            /// </summary>
            public HashSet<Symbol> Symbols
            {
                get;
                internal set;
            }

            private Kind _kind;
            private Node _node;
            /// <summary>
            /// The Programto which _node belongs
            /// </summary>
            private ProgramSymbol _program;
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="kind">The Kind of storage area requested</param>
            /// <param name="node"></param>
            public StorageAreaVisitor(Kind kind, Node node)
            {
                Symbols = new HashSet<Symbol>();
                _kind = kind;
                _node = node;
                //Check that a semantic data has been associated to this node.
                System.Diagnostics.Debug.Assert(node.SemanticData != null);
                System.Diagnostics.Debug.Assert(node.SemanticData.SemanticKind == SemanticKinds.Symbol);

                //The semantic data must be a ProgramSymbol or a FunctionSymbol
                System.Diagnostics.Debug.Assert(((Symbol)node.SemanticData).Kind == Symbol.Kinds.Program ||
                                                ((Symbol)node.SemanticData).Kind == Symbol.Kinds.Function);

                _program = (ProgramSymbol)node.SemanticData;
            }
            /// <summary>
            /// Visitor on  StorageArea
            /// </summary>
            /// <param name="storageArea"></param>
            /// <returns></returns>
            public override bool Visit(StorageArea storageArea)
            {
                SymbolReference symRef = storageArea.SymbolReference;
                //Resolve the Symbol Reference.
                Container<VariableSymbol>.Entry result = _program.ResolveReference(symRef, true);
                System.Diagnostics.Debug.Assert(result != null);
                if (result.Count == 1)
                {//We have found an unique match
                    if ((storageArea.IsReadFrom && (_kind & Kind.Read) != 0) || (storageArea.IsWrittenTo && (_kind & Kind.Write) != 0))
                    {
                        Symbols.Add(result.Symbol);
                    }
                }
                return true;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg"></param>
        public TypeCobolDataFlowGraphBuilder(ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>> cfg) : base(cfg)
        {
        }

        /// <summary>
        /// Get Use Variables for a given node.
        /// </summary>
        /// <param name="node">The node</param>
        /// <returns>The set of used variables</returns>
        public override HashSet<Symbol> GetUseVariables(Node node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            StorageAreaVisitor visitor = new StorageAreaVisitor(StorageAreaVisitor.Kind.Read, node);
            node.CodeElement?.AcceptASTVisitor(visitor);
            return visitor.Symbols;
        }

        /// <summary>
        /// Get Defined Variables for a given node
        /// </summary>
        /// <param name="node">The node</param>
        /// <returns>The set of defined variable</returns>
        public override HashSet<Symbol> GetDefVariables(Node node)
        {
            System.Diagnostics.Debug.Assert(node != null);
            StorageAreaVisitor visitor = new StorageAreaVisitor(StorageAreaVisitor.Kind.Write, node);
            node.CodeElement?.AcceptASTVisitor(visitor);
            return visitor.Symbols;
        }
    }
}

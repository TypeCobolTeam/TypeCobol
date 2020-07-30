using System;
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
    public class DefaultDataFlowGraphBuilder : DataFlowGraphBuilder<Node, Symbol>
    {
        /// <summary>
        /// Visitor on StorageArea to collect them.
        /// </summary>
        internal class StorageAreaVisitor : TypeCobol.Compiler.CodeElements.AbstractAstVisitor
        {
            /// <summary>
            /// Kind of Storage Area Requested
            /// </summary>
            [Flags]
            internal enum Kind : byte
            {
                Read = 1,
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

            private readonly Kind _kind;
            private Node _node;
            private readonly IDictionary<StorageArea, VariableSymbol> _storageAreaReadsSymbol;
            private readonly IDictionary<StorageArea, VariableSymbol> _storageAreaWritesSymbol;
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
                _storageAreaReadsSymbol = node.StorageAreaReadsSymbol;
                _storageAreaWritesSymbol = node.StorageAreaWritesSymbol;

        }
        /// <summary>
        /// Visitor on StorageArea
        /// </summary>
        /// <param name="storageArea"></param>
        /// <returns></returns>
        public override bool Visit(StorageArea storageArea)
            {
                SymbolReference symRef = storageArea.SymbolReference;
                if (_storageAreaReadsSymbol != null  && storageArea.IsReadFrom && (_kind & Kind.Read) != 0)
                {
                    if (_storageAreaReadsSymbol.TryGetValue(storageArea, out var symbol))
                    {
                        Symbols.Add(symbol);
                    }
                }
                if (_storageAreaWritesSymbol != null && storageArea.IsWrittenTo && (_kind & Kind.Write) != 0)
                {
                    if (_storageAreaWritesSymbol.TryGetValue(storageArea, out var symbol))
                    {
                        Symbols.Add(symbol);
                    }
                }
                return true;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg"></param>
        public DefaultDataFlowGraphBuilder(ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>> cfg) : base(cfg)
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

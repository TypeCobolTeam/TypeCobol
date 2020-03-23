using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Domain
{
    /// <summary>
    /// Abstract base Class use to build the Symbol Table from a program perspective;
    /// </summary>
    public abstract class SymbolTableBuilder : ProgramClassBuilderNodeListener
    {
        /// <summary>
        /// Path of the Intrinsic file
        /// </summary>
        public static string IntrinsicPath
        {
            get;
            set;
        }

        /// <summary>
        /// Called when A node has been syntactically recognized by the TypeCobol Parser.
        /// </summary>
        /// <param name="node">The node being built</param>
        /// <param name="program">The Program that contains the node.</param>
        public abstract override void OnNode(Node node, Program program);

        /// <summary>
        /// Add in the given RootSymbolTable instance all Builtin symbols
        /// </summary>
        /// <param name="root">The RootSymbolTable instance</param>
        public static void AddBuiltinSymbol(RootSymbolTable root)
        {
            foreach (var type in BuiltinSymbols.All)
            {
                root.Add(type);
            }
        }
    }
}

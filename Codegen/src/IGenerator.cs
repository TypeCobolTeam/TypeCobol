using System;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// The Interface of Any Generator
    /// </summary>
    public interface IGenerator
    {
        /// <summary>
        /// The Code Generation method.
        /// </summary>
        /// <param name="tree">Root of a syntax tree</param>
        /// <param name="table">Table of symbols</param>
        /// <param name="columns">Columns layout</param>
        void Generate(Root tree, SymbolTable table, ColumnsLayout columns = ColumnsLayout.FreeTextFormat);
    }
}

using System;
using System.Collections.Generic;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
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
        /// <param name="compilationUnit">Compilation unit resulting from TypeCobol parsing</param>
        /// <param name="columns">Columns layout</param>
        void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat);

        List<Diagnostic> Diagnostics { get; }
    }
}

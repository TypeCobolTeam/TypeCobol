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

        IReadOnlyDictionary<string, TimeSpan> PerformanceReport { get; }

        string TypeCobolVersion { get; set; }

        /// <summary>
        /// Determine if this generator has line Map data.
        /// </summary>
        bool HasLineMapData { get; }

        /// <summary>
        /// Generate any line map data in the given stream.
        /// </summary>
        /// <param name="stream">The Stream into which to generate the Line Mapping data</param>
        /// <exception cref="IOException">If an Input/Output exception is raide.</exception>
        void GenerateLineMapFile(System.IO.Stream stream);
    }
}

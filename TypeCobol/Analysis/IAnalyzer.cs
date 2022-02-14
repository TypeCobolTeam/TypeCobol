using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Base interface for any analyzer.
    /// It must be extended to further precise the role of the analyzer in the compilation process.
    /// </summary>
    public interface IAnalyzer
    {
        /// <summary>
        /// Unique text identifier of this analyzer.
        /// </summary>
        [NotNull]
        string Identifier { get; }

        /// <summary>
        /// Returns the diagnostics produced by this analyzer.
        /// May be empty, but not null.
        /// </summary>
        [NotNull]
        IEnumerable<Diagnostic> Diagnostics { get; }

        /// <summary>
        /// Returns the result produced by this analyzer.
        /// Contract between callers and analyzers is implicit, analyzer may return ANY object.
        /// </summary>
        /// <returns>Result object, may be null if the analyzer produces only diagnostics.</returns>
        object GetResult();
    }
}

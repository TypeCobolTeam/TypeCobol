using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// Base class to help implement an ISyntaxDrivenAnalyzer.
    /// All methods from <code>INodeListener</code> and <code>IProgramClassBuilder</code> have empty implementations
    /// and can be overriden. This base class also supports Diagnostics property from <code>IAnalyzer</code>.
    /// </summary>
    public abstract class SyntaxDrivenAnalyzerBase : ProgramClassBuilderNodeListener, ISyntaxDrivenAnalyzer
    {
        private List<Diagnostic> _diagnostics;
        protected List<Diagnostic> DiagnosticList => _diagnostics ?? (_diagnostics = new List<Diagnostic>());

        /// <summary>
        /// Returns the diagnostics produced by this analyzer.
        /// May be empty, but not null.
        /// </summary>
        public IEnumerable<Diagnostic> Diagnostics => DiagnosticList.AsEnumerable();

        /// <summary>
        /// Unique text identifier of this analyzer.
        /// </summary>
        public string Identifier { get; }

        protected SyntaxDrivenAnalyzerBase([NotNull] string identifier)
        {
            Identifier = identifier;
        }

        /// <summary>
        /// Returns the result produced by this analyzer.
        /// Contract between callers and analyzers is implicit, analyzer may return ANY object.
        /// </summary>
        /// <returns>Result object, may be null if the analyzer produces only diagnostics.</returns>
        public abstract object GetResult();
    }
}

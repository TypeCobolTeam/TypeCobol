using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Base class to help implement an IASTAnalyzer.
    /// </summary>
    public abstract class ASTAnalyzerBase : AbstractAstVisitor, IASTAnalyzer
    {
        private List<Diagnostic> _diagnostics;

        /// <summary>
        /// Unique text identifier of this analyzer.
        /// </summary>
        public string Identifier { get; }

        protected ASTAnalyzerBase([NotNull] string identifier)
        {
            Identifier = identifier;
        }

        /// <summary>
        /// Returns the diagnostics produced by this analyzer.
        /// May be empty, but not null.
        /// </summary>
        public IEnumerable<Diagnostic> Diagnostics => _diagnostics ?? Enumerable.Empty<Diagnostic>();

        /// <summary>
        /// Returns the result produced by this analyzer.
        /// Contract between callers and analyzers is implicit, analyzer may return ANY object.
        /// </summary>
        /// <returns>Result object, may be null if the analyzer produces only diagnostics.</returns>
        public abstract object GetResult();

        /// <summary>
        /// Add a diagnostic.
        /// </summary>
        /// <param name="diagnostic">Non-null diagnostic instance.</param>
        protected void AddDiagnostic([NotNull] Diagnostic diagnostic)
        {
            if (_diagnostics == null)
            {
                _diagnostics = new List<Diagnostic>();
            }

            _diagnostics.Add(diagnostic);
        }
    }
}

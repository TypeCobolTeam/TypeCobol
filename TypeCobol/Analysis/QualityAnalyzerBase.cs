using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Base class to help implement a IQualityAnalyzer.
    /// </summary>
    public abstract class QualityAnalyzerBase : IQualityAnalyzer
    {
        private List<Diagnostic> _diagnostics;

        /// <summary>
        /// Unique text identifier of this analyzer.
        /// </summary>
        public string Identifier { get; }

        protected QualityAnalyzerBase([NotNull] string identifier)
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

        /// <summary>
        /// Perform inspection of TokensDocument.
        /// </summary>
        /// <param name="tokensDocument">Current snapshot of tokens.</param>
        public virtual void Inspect(TokensDocument tokensDocument)
        {

        }

        /// <summary>
        /// Perform inspection of ProcessedTokensDocument.
        /// </summary>
        /// <param name="processedTokensDocument">Current snapshot of tokens after preprocessor step.</param>
        public virtual void Inspect(ProcessedTokensDocument processedTokensDocument)
        {

        }

        /// <summary>
        /// Perform inspection of Code Elements.
        /// </summary>
        /// <param name="codeElementsDocument">Current snapshot of code elements.</param>
        public virtual void Inspect(CodeElementsDocument codeElementsDocument)
        {

        }

        /// <summary>
        /// Perform inspection of Nodes (before cross-check).
        /// </summary>
        /// <param name="temporarySemanticDocument">Current partial semantic document.</param>
        public virtual void Inspect(TemporarySemanticDocument temporarySemanticDocument)
        {

        }

        /// <summary>
        /// Perform inspection of Nodes (after cross-check).
        /// </summary>
        /// <param name="programClassDocument">Current full semantic document.</param>
        public virtual void Inspect(ProgramClassDocument programClassDocument)
        {

        }
    }
}

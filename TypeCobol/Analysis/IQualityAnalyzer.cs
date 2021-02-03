using JetBrains.Annotations;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Contract for a quality analyzer.
    /// </summary>
    public interface IQualityAnalyzer : IAnalyzer
    {
        /// <summary>
        /// Perform inspection of TokensDocument.
        /// </summary>
        /// <param name="tokensDocument">Current snapshot of tokens.</param>
        void Inspect([NotNull] TokensDocument tokensDocument);

        /// <summary>
        /// Perform inspection of ProcessedTokensDocument.
        /// </summary>
        /// <param name="processedTokensDocument">Current snapshot of tokens after preprocessor step.</param>
        void Inspect([NotNull] ProcessedTokensDocument processedTokensDocument);

        /// <summary>
        /// Perform inspection of Code Elements.
        /// </summary>
        /// <param name="codeElementsDocument">Current snapshot of code elements.</param>
        void Inspect([NotNull] CodeElementsDocument codeElementsDocument);

        /// <summary>
        /// Perform inspection of Nodes (before cross-check).
        /// </summary>
        /// <param name="temporarySemanticDocument">Current partial semantic document.</param>
        void Inspect([NotNull] TemporarySemanticDocument temporarySemanticDocument);

        /// <summary>
        /// Perform inspection of Nodes (after cross-check).
        /// </summary>
        /// <param name="programClassDocument">Current full semantic document.</param>
        void Inspect([NotNull] ProgramClassDocument programClassDocument);
    }
}

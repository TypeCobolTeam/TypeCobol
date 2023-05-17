#nullable enable

using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// List of tokens and diagnostics found by scanning one line of text
    /// </summary>
    public interface ITokensLine : ICobolTextLine
    {
        /// <summary>
        /// Tokens found while scanning the raw source text line
        /// (before the text manipulation phase)
        /// </summary>
        IList<Token> SourceTokens { get; }

        /// <summary>
        /// Error and warning messages produced while scanning the raw source text line
        /// (before text manipulation phase)
        /// </summary>
        IList<Diagnostic> ScannerDiagnostics { get; }

        /// <summary>
        /// True if the first token on the next line continues the last token of this line
        /// </summary>
        bool HasTokenContinuedOnNextLine { get; }

        /// <summary>
        /// True if the first token on this line continues the last token of the previous line
        /// </summary>
        bool HasTokenContinuationFromPreviousLine { get; }

        /// <summary>
        /// Original ScanState before scanning the line
        /// </summary>
        MultilineScanState? InitialScanState { get; }

        /// <summary>
        /// Internal state used by the Scanner to disambiguate context-sensitive keywords
        /// </summary>
        MultilineScanState? ScanState { get; }

        /// <summary>
        /// Collect all diagnostics attached to this line into the given list.
        /// </summary>
        /// <param name="diagnostics">A non-null list of diagnostics to accumulate results</param>
        void CollectDiagnostics(List<Diagnostic> diagnostics);
    }
}
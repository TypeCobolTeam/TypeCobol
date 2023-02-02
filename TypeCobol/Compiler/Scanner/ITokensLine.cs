using System.Collections.Generic;
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
        /// Internal state used by the Scanner to disambiguate context-sensitive keywords
        /// </summary>
        MultilineScanState ScanState { get; }

        /// <summary>
        /// Enumerate all diagnostic for this line.
        /// </summary>
        /// <returns>A (potentially deferred) enumeration of all diagnostics attached to this line.</returns>
        IEnumerable<Diagnostic> AllDiagnostics();
    }
}
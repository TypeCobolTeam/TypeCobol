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
    }
}
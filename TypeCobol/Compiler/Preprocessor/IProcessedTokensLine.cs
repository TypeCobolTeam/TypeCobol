using System.Collections.Generic;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Line of tokens after preprocessor execution
    /// </summary>
    public interface IProcessedTokensLine : ITokensLine
    {
        /// <summary>
        /// True if compiler directives have been recognized on the current line
        /// (true on each line for multiline compiler directives)
        /// </summary>
        bool HasCompilerDirectives { get; }

        /// <summary>
        /// Tokens produced after parsing the compiler directives.
        /// If a compiler directive is found, several tokens of the source text are grouped 
        /// into one single CompilerDirectiveToken (which can be continued on several lines).
        /// </summary>
        IList<Token> TokensWithCompilerDirectives { get; }

        /// <summary>
        /// Compiler listing control directive found on the current line
        /// *CBL or *CONTROL, EJECT, SKIP1 or SKIP2 or SKIP3, TITLE
        /// (these compiler directives can't span several lines, and you can only write one of them per line)
        /// </summary>
        CompilerDirective CompilerListingControlDirective { get; }

        /// <summary>
        /// Imported compilation documents for each COPY directive found (starting) on this line
        /// </summary>
        IDictionary<CopyDirective, ImportedTokensDocument> ImportedDocuments { get; }

        /// <summary>
        /// Error and warning messages produced while scanning the raw source text line
        /// (before text manipulation phase)
        /// </summary>
        IList<Diagnostic> PreprocessorDiagnostics { get; }
    }
}
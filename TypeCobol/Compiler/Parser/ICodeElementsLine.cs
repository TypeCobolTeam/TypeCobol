using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Preprocessor;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Line of code elements after parser execution
    /// </summary>
    public interface ICodeElementsLine : IProcessedTokensLine
    {
        /// <summary>
        /// Code elements STARTING on this line 
        /// </summary>
        IList<CodeElement> CodeElements { get; }

        /// <summary>
        /// True if a code element starts on the current line
        /// </summary>
        bool HasCodeElements { get; }

        /// <summary>
        /// Error and warning messages produced while parsing the source text line
        /// </summary>
        IList<Diagnostic> ParserDiagnostics { get; }
    }
}

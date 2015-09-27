using System;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// List of the incremental compiler processing steps
    /// </summary>
    public enum CompilationStep
    {
        /// <summary>
        /// Raw text lines are partitioned in the Cobol reference format areas
        /// </summary>
        Text,
        /// <summary>
        /// Cobol formatted lines are scanned as a lists of tokens
        /// </summary>
        Scanner,
        /// <summary>
        /// Lists of tokens are  processed as compiler directives (including COPY and REPLACE)
        /// </summary>
        Preprocessor,
        /// <summary>
        /// Tokens transformed by COPY and REPLACE directives are parsed as CodeElement objects
        /// </summary>
        CodeElementsParser,
        /// <summary>
        /// Individual CodeElement objects are parsed in a coherent tree to form a Program or Class
        /// </summary>
        ProgramClassParser
    }
}

using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The EXEC statement enables the developer to embed statements written
    /// in another programming languages in a Cobol Program.
    /// The Cobol compiler must delegate the analysis and translation of these
    /// embedded statements to secondary compilers, implemented as preprocessors
    /// in older versions of IBM Cobol, or coprocessors in more recent versions
    /// (with better integration in the Cobol compiler).
    /// </summary>
    public class ExecStatement : StatementElement
    {
        public ExecStatement() : base(CodeElementType.ExecStatement, StatementType.ExecStatement)
        { }
        
        /// <summary>
        /// Secondary compiler to delegate the analysis of Code to.
        /// </summary>
        public string Compiler;
        /// <summary>
        /// Source code to be analyzed by Compiler
        /// </summary>
        public string Code;
    }
}

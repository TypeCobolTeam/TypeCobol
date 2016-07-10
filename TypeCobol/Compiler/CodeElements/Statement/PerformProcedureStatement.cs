using System;

namespace TypeCobol.Compiler.CodeElements
{       
    /// <summary>
    /// p384: PERFORM statement
    /// The PERFORM statement transfers control explicitly to one or more procedures
    /// and implicitly returns control to the next executable statement after execution of
    /// the specified procedures is completed.
    /// </summary>
    public class PerformProcedureStatement : PerformStatement
    {
        public PerformProcedureStatement() : base(CodeElementType.PerformProcedureStatement, StatementType.PerformProcedureStatement)
        { }

        /// <summary>
        /// Whenever an out-of-line PERFORM statement is executed, control is transferred to
        /// the first statement of the procedure named procedure-name-1. Control is always
        /// returned to the statement following the PERFORM statement. The point from
        /// which this control is returned is determined as follows:
        /// - If procedure-name-1 is a paragraph name and procedure-name-2 is not specified, the
        ///   return is made after the execution of the last statement of the procedure-name-1
        ///   paragraph.
        /// - If procedure-name-1 is a section name and procedure-name-2 is not specified, the
        ///   return is made after the execution of the last statement of the last paragraph in
        ///   the procedure-name-1 section.        
        /// </summary>
        public SymbolReference Procedure { get; set; }

        /// <summary>
        /// - If procedure-name-2 is specified and it is a paragraph name, the return is made
        ///   after the execution of the last statement of the procedure-name-2 paragraph.
        /// - If procedure-name-2 is specified and it is a section name, the return is made after
        ///   the execution of the last statement of the last paragraph in the procedure-name-2
        ///   section.
        /// The only necessary relationship between procedure-name-1 and procedure-name-2 is
        /// that a consecutive sequence of operations is executed, beginning at the procedure
        /// named by procedure-name-1 and ending with the execution of the procedure named
        /// by procedure-name-2.
        /// </summary>
        public SymbolReference ThroughProcedure { get; set; }
    }
}

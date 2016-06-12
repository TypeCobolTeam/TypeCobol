using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p384: PERFORM statement
    /// The PERFORM statement transfers control explicitly to one or more procedures
    /// and implicitly returns control to the next executable statement after execution of
    /// the specified procedures is completed.
    /// </summary>
    public class PerformProcedureStatement : StatementElement
    {
        public PerformProcedureStatement() : base(CodeElementType.PerformProcedureStatement, StatementType.PerformProcedureStatement)
        { }
    }
}

using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p331: EVALUATE statement
    /// The EVALUATE statement provides a shorthand notation for a series of nested IF statements. 
    /// The EVALUATE statement can evaluate multiple conditions.
    /// The subsequent action depends on the results of these evaluations.
    /// </summary>
    public class EvaluateStatement : StatementElement
    {
        public EvaluateStatement() : base(CodeElementType.EvaluateStatement, StatementType.EvaluateStatement)
        { }
    }
}

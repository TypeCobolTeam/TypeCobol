using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p335: EXIT statement
    /// The EXIT statement provides a common end point for a series of procedures.
    /// The EXIT statement enables you to assign a procedure-name to a given point
    /// in a program.
    /// The EXIT statement is treated as a CONTINUE statement. Any statements
    /// following the EXIT statement are executed.
    /// </summary>
    public class ExitStatement : StatementElement
    {
        public ExitStatement() : base(CodeElementType.ExitStatement, StatementType.ExitStatement) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
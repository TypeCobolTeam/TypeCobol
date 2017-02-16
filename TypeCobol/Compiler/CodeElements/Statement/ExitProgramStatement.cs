using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p337: EXIT PROGRAM statement
    /// The EXIT PROGRAM statement specifies the end of a called program and returns control to the calling program.
    /// </summary>
    public class ExitProgramStatement : StatementElement
    {
        public ExitProgramStatement() : base(CodeElementType.ExitProgramStatement, StatementType.ExitProgramStatement)
        { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}

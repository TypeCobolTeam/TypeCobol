using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SetAssignmentStatement : SqlStatementElement
    {
        public IList<Assignment> Assignments { get; }
        public SetAssignmentStatement(IList<Assignment> assignments) : base(CodeElementType.SetAssignmentStatement, StatementType.SetAssignmentStatement)
        {
            Assignments = assignments;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(Assignments);
        }

    }
}

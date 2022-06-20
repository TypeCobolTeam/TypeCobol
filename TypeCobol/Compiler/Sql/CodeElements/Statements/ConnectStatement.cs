using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class ConnectStatement : SqlStatementElement
    {
        public ConnectionTarget Target { get; }
        public SyntaxProperty<bool> Reset { get; }
        public ConnectionAuthorization Authorization { get; }

        public ConnectStatement(ConnectionAuthorization authorization, SyntaxProperty<bool> reset, ConnectionTarget target) : base(
            CodeElementType.ConnectStatement, StatementType.ConnectStatement)
        {
            Authorization = authorization;
            Reset = reset;
            Target = target;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, Reset)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(Target, Authorization);
        }
    }
}
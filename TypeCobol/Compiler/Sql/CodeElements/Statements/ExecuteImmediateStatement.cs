using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class ExecuteImmediateStatement : SqlStatementElement
    {
        public SqlExpression Expression { get; }

        public ExecuteImmediateStatement(SqlExpression expression) : base(CodeElementType.ExecuteImmediateStatement, StatementType.ExecuteImmediateStatement)
        {
            Expression = expression;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(Expression);
        }
    }
}

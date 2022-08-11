using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class ExecuteImmediateStatement : SqlStatementElement
    {
        public SqlVariable StatementVariable { get; }
        public StringExpression StatementExpression { get; }

        public ExecuteImmediateStatement(SqlVariable statementVariable, StringExpression statementExpression)
            : base(CodeElementType.ExecuteImmediateStatement, StatementType.ExecuteImmediateStatement)
        {
            StatementVariable = statementVariable;
            StatementExpression = statementExpression;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(StatementVariable, StatementExpression);
        }
    }
}

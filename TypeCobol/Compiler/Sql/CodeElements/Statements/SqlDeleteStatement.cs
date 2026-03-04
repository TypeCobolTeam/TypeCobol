using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL DELETE Statement Code Element.
    /// </summary>
    public class SqlDeleteStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }
        public bool HasWhereClause { get; }

        public SqlDeleteStatement(TableViewCorrelationName tableName, bool hasWhereClause)
            : base(CodeElementType.SqlDeleteStatement, StatementType.SqlDeleteStatement)
        {
            TableName = tableName;
            HasWhereClause = hasWhereClause;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName);
        }
    }
}

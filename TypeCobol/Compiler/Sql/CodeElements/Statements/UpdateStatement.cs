using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL UPDATE Statement Code Element.
    /// </summary>
    public class UpdateStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }
        public bool HasWhereClause { get; }

        public UpdateStatement(TableViewCorrelationName tableName, bool hasWhereClause)
            : base(CodeElementType.UpdateStatement, StatementType.UpdateStatement)
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

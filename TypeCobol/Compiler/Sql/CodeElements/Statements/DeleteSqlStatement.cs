using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL DELETE Statement Code Element.
    /// EXEC SQL DELETE FROM table-name [WHERE condition] END-EXEC
    /// Named DeleteSqlStatement to avoid conflict with COBOL DELETE statement.
    /// </summary>
    public class DeleteSqlStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }

        public DeleteSqlStatement(TableViewCorrelationName tableName)
            : base(CodeElementType.DeleteSqlStatement, StatementType.DeleteSqlStatement)
        {
            TableName = tableName;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName);
        }
    }
}

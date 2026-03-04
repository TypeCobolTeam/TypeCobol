using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL INSERT Statement Code Element.
    /// </summary>
    public class InsertStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }
        public FullSelect FullSelect { get; }

        public InsertStatement(TableViewCorrelationName tableName, FullSelect fullSelect)
            : base(CodeElementType.InsertStatement, StatementType.InsertStatement)
        {
            TableName = tableName;
            FullSelect = fullSelect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName)
                                                     && astVisitor.SqlVisitor.ContinueVisit(FullSelect);
        }
    }
}

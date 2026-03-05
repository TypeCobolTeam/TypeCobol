using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL INSERT Statement Code Element.
    /// EXEC SQL INSERT INTO table-name [(column-list)] VALUES (value-list) | fullselect END-EXEC
    /// </summary>
    public class InsertStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }
        public IList<SqlColumnName> Columns { get; }
        public IList<SqlExpression> Values { get; }
        public FullSelect FullSelect { get; }

        public InsertStatement(TableViewCorrelationName tableName, IList<SqlColumnName> columns, IList<SqlExpression> values, FullSelect fullSelect)
            : base(CodeElementType.InsertStatement, StatementType.InsertStatement)
        {
            TableName = tableName;
            Columns = columns;
            Values = values;
            FullSelect = fullSelect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName, FullSelect);
        }
    }
}

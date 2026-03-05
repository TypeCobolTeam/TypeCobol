using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL UPDATE Statement Code Element.
    /// EXEC SQL UPDATE table-name SET column = expr [, ...] [WHERE condition] END-EXEC
    /// </summary>
    public class UpdateStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }
        public IList<Assignment> Assignments { get; }

        public UpdateStatement(TableViewCorrelationName tableName, IList<Assignment> assignments)
            : base(CodeElementType.UpdateStatement, StatementType.UpdateStatement)
        {
            TableName = tableName;
            Assignments = assignments;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName);
        }
    }
}

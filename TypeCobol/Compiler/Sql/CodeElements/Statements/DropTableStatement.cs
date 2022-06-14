using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class DropTableStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableOrAliasName { get; }

        public DropTableStatement(TableViewCorrelationName tableOrAliasName) : base(CodeElementType.DropTableStatement,
            StatementType.DropTableStatement)
        {
            TableOrAliasName = tableOrAliasName;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableOrAliasName);

        }
    }
}

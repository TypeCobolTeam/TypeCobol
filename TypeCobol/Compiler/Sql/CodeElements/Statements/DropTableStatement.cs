using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class DropTableStatement : SqlStatementElement
    {
        public TableViewCorrelationName TableName { get; }
        public SymbolReference AliasName { get; }

        public DropTableStatement(TableViewCorrelationName tableName) : base(CodeElementType.DropTableStatement,
            StatementType.DropTableStatement)
        {
            TableName = tableName;
        }

        public DropTableStatement(SymbolReference aliasName) : base(CodeElementType.DropTableStatement,
            StatementType.DropTableStatement)
        {
            AliasName = aliasName;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, AliasName)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName);

        }
    }
}

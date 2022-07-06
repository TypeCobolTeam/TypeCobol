using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class DropTableStatement : SqlStatementElement
    {
        public SymbolReference TableOrAliasName { get; }

        public DropTableStatement(SymbolReference tableOrAliasName) : base(CodeElementType.DropTableStatement,
            StatementType.DropTableStatement)
        {
            TableOrAliasName = tableOrAliasName;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.Visit(TableOrAliasName);

        }
    }
}

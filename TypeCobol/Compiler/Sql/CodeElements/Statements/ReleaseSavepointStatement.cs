using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class ReleaseSavepointStatement : SqlStatementElement
    {
        public SymbolReference SavepointName { get; }

        public ReleaseSavepointStatement(SymbolReference savepointName) : base(
            CodeElementType.ReleaseSavepointStatement, StatementType.ReleaseSavepointStatement)
        {
            SavepointName = savepointName;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, SavepointName);
        }
    }
}

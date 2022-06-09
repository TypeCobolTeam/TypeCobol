using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SavepointStatement : SqlStatementElement
    {
        public SymbolReference SavepointName { get; }
        public SyntaxProperty<bool> RetainLocks { get; }
        public SyntaxProperty<bool> IsUnique { get; }

        public SavepointStatement(SymbolReference savepointName, SyntaxProperty<bool> retainLocks, SyntaxProperty<bool> isUnique) : base(CodeElementType.SavepointStatement, StatementType.SavepointStatement)
        {
            SavepointName = savepointName;
            RetainLocks = retainLocks;
            IsUnique = isUnique;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, SavepointName,
                                                         RetainLocks, IsUnique);
        }
    }
}

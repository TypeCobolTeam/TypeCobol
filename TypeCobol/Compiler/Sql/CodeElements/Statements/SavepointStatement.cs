using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SavepointStatement : SqlStatementElement
    {
        public SymbolReference Name { get; }
        public SyntaxProperty<bool> RetainLocks { get; }
        public SyntaxProperty<bool> IsUnique { get; }

        public SavepointStatement(SymbolReference name, SyntaxProperty<bool> retainLocks, SyntaxProperty<bool> isUnique) : base(CodeElementType.SavepointStatement, StatementType.SavepointStatement)
        {
            Name = name;
            RetainLocks = retainLocks;
            IsUnique = isUnique;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, Name,
                                                         RetainLocks, IsUnique);
        }
    }
}

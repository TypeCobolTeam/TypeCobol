using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL CLOSE Statement Code Element.
    /// </summary>
    public class SqlCloseStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }

        public SqlCloseStatement(SymbolReference cursorName)
            : base(CodeElementType.SqlCloseStatement, StatementType.SqlCloseStatement)
        {
            CursorName = cursorName;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, CursorName);
        }
    }
}

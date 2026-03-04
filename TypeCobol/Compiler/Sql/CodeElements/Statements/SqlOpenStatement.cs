using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL OPEN Statement Code Element.
    /// </summary>
    public class SqlOpenStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }

        public SqlOpenStatement(SymbolReference cursorName)
            : base(CodeElementType.SqlOpenStatement, StatementType.SqlOpenStatement)
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

using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL DECLARE CURSOR Statement Code Element.
    /// </summary>
    public class DeclareCursorStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }
        public FullSelect FullSelect { get; }
        public bool WithHold { get; }

        public DeclareCursorStatement(SymbolReference cursorName, FullSelect fullSelect, bool withHold)
            : base(CodeElementType.DeclareCursorStatement, StatementType.DeclareCursorStatement)
        {
            CursorName = cursorName;
            FullSelect = fullSelect;
            WithHold = withHold;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, CursorName)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(FullSelect);
        }
    }
}

using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL DECLARE CURSOR Statement Code Element.
    /// EXEC SQL DECLARE cursor-name CURSOR [WITH HOLD] [WITH RETURN] FOR fullselect END-EXEC
    /// </summary>
    public class DeclareCursorStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }
        public SyntaxProperty<bool> WithHold { get; }
        public SyntaxProperty<bool> WithReturn { get; }
        public FullSelect FullSelect { get; }

        public DeclareCursorStatement(SymbolReference cursorName, SyntaxProperty<bool> withHold, SyntaxProperty<bool> withReturn, FullSelect fullSelect)
            : base(CodeElementType.DeclareCursorStatement, StatementType.DeclareCursorStatement)
        {
            CursorName = cursorName;
            WithHold = withHold;
            WithReturn = withReturn;
            FullSelect = fullSelect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, WithHold, WithReturn)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(FullSelect);
        }
    }
}

using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class DeclareCursorStatement : SqlStatementElement
    {
        public string CursorName { get; }
        public bool WithHold { get; }
        public bool WithReturn { get; }
        public FullSelect InnerSelect { get; }
        public string StatementName { get; }

        public DeclareCursorStatement(string cursorName, FullSelect innerSelect, string statementName = null, bool withHold = false, bool withReturn = false)
            : base(CodeElementType.DeclareCursorStatement, StatementType.DeclareCursorStatement)
        {
            CursorName = cursorName;
            InnerSelect = innerSelect;
            StatementName = statementName;
            WithHold = withHold;
            WithReturn = withReturn;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && (InnerSelect == null || astVisitor.SqlVisitor == null || astVisitor.SqlVisitor.ContinueVisit(InnerSelect));
        }
    }
}

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL FETCH Statement Code Element.
    /// </summary>
    public class FetchStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }
        public IList<SqlVariable> IntoVariables { get; }

        public FetchStatement(SymbolReference cursorName, IList<SqlVariable> intoVariables)
            : base(CodeElementType.FetchStatement, StatementType.FetchStatement)
        {
            CursorName = cursorName;
            IntoVariables = intoVariables;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, CursorName);
        }
    }
}

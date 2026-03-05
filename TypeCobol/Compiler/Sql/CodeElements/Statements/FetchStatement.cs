using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL FETCH Statement Code Element.
    /// EXEC SQL FETCH cursor-name INTO :host-var [, :host-var ...] END-EXEC
    /// </summary>
    public class FetchStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }
        public IList<HostVariable> IntoVariables { get; }

        public FetchStatement(SymbolReference cursorName, IList<HostVariable> intoVariables)
            : base(CodeElementType.FetchStatement, StatementType.FetchStatement)
        {
            CursorName = cursorName;
            IntoVariables = intoVariables;
        }
    }
}

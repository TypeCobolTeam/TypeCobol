using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL CLOSE cursor Statement Code Element.
    /// EXEC SQL CLOSE cursor-name END-EXEC
    /// </summary>
    public class CloseCursorStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }

        public CloseCursorStatement(SymbolReference cursorName)
            : base(CodeElementType.CloseCursorStatement, StatementType.CloseCursorStatement)
        {
            CursorName = cursorName;
        }
    }
}

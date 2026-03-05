using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL OPEN cursor Statement Code Element.
    /// EXEC SQL OPEN cursor-name [USING host-variable [, ...]] END-EXEC
    /// </summary>
    public class OpenCursorStatement : SqlStatementElement
    {
        public SymbolReference CursorName { get; }

        public OpenCursorStatement(SymbolReference cursorName)
            : base(CodeElementType.OpenCursorStatement, StatementType.OpenCursorStatement)
        {
            CursorName = cursorName;
        }
    }
}

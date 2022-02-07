using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.SqlCodeElements.Statement
{
    /// <summary>
    /// SQL COMMIT Statement Code Element.
    /// </summary>
    public class CommitStatement : SqlStatementElement
    {
        public CommitStatement() : base(CodeElementType.CommitStatement, StatementType.CommitStatement)
        {
        }
    }
}

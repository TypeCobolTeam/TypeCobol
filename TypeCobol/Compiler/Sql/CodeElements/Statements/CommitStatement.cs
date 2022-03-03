using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
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

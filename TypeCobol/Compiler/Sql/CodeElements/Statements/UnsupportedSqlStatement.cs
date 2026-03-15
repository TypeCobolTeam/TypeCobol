using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// Represents an SQL statement that is not supported by the grammar
    /// (e.g., OPEN, FETCH, CLOSE).
    /// </summary>
    public class UnsupportedSqlStatement : SqlStatementElement
    {
        /// <summary>
        /// The SQL keyword that started this unsupported statement.
        /// </summary>
        public string SqlKeyword { get; }

        public UnsupportedSqlStatement(string sqlKeyword)
            : base(CodeElementType.UnsupportedSqlStatement, StatementType.UnsupportedSqlStatement)
        {
            SqlKeyword = sqlKeyword;
        }
    }
}

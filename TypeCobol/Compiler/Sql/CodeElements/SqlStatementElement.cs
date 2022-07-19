using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    /// <summary>
    /// Base class for all Sql statement CodeElements
    /// </summary>
    public abstract class SqlStatementElement : StatementElement
    {
        /// <summary>
        /// This method lists tokens that marks the beginning of a new SQL statement.
        /// It is used by ANTLR recovery strategy and must be updated each time a new statement is introduced !
        /// </summary>
        /// <param name="tokenType">TokenType to test</param>
        /// <returns>True if the keyword is a starting keyword for SQL statement, False otherwise.</returns>
        public static bool IsSqlStatementStartingKeyword(TokenType tokenType)
        {
            switch (tokenType)
            {
                case TokenType.SQL_COMMIT:
                case TokenType.SQL_ROLLBACK:
                case TokenType.SQL_SELECT:
                case TokenType.SQL_SAVEPOINT:
                case TokenType.SQL_WHENEVER:
                case TokenType.SQL_LOCK:
                case TokenType.SQL_RELEASE:
                case TokenType.SQL_DROP:
                case TokenType.SQL_SET:
                    return true;
                default:
                    return false;
            }
        }

        protected SqlStatementElement(CodeElementType codeElementType, StatementType statementType) 
            : base(codeElementType, statementType)
        {
        }
    }
}

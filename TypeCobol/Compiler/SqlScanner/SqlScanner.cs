using System;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// A Sql Scanner related to a Scanner
    /// </summary>
    public class SqlScanner : AbstractScanner
    {
        /// <summary>
        /// True if the given char can be part of an SQL keyword.
        /// </summary>
        /// <param name="c">The character to test</param>
        /// <returns>True if yes, no otherwise</returns>
        public static bool IsSqlKeywordPart(char c)
        {
            return c == '-' || char.IsLetter(c);
        }

        private readonly string _line;
        private readonly int _lastIndex;
        private readonly TokensLine _tokensLine;
        private readonly TypeCobolOptions _compilerOptions;

        public SqlScanner(string line, int lastIndex, TokensLine tokensLine, TypeCobolOptions compilerOptions)
            : base()
        {
            _line = line;
            _lastIndex = lastIndex;
            _tokensLine = tokensLine;
            _compilerOptions = compilerOptions;
        }

        public Token GetNextToken(ref int currentIndex)
        {
            if (currentIndex < 0 || currentIndex > _lastIndex)
            {
                throw new ArgumentOutOfRangeException(nameof(currentIndex), "Start index must be positive and cannot exceed last index");
            }

            int startIndex = currentIndex;

            if (IsSqlKeywordPart(_line[currentIndex]))
            {
                //Consume all sql-keyword compatible chars
                for (; currentIndex <= _lastIndex && IsSqlKeywordPart(_line[currentIndex]); currentIndex++) { }
                string tokenText = _line.Substring(startIndex, currentIndex - startIndex);

                //Try to match keyword text
                var tokenType = TokenUtils.GetSqlKeywordTokenTypeFromTokenString(tokenText);

                //So far this scanner only recognize 'COMMIT' keyword
                if (tokenType == TokenType.SQL_COMMIT)
                {
                    return new Token(TokenType.SQL_COMMIT, startIndex, currentIndex - 1, _tokensLine);
                }

                //Unrecognized keyword (for now) return as ExecStatementText
                return new Token(TokenType.ExecStatementText, startIndex, currentIndex - 1, _tokensLine);
            }

            //Consume all sql-keyword incompatible chars
            for (; currentIndex <= _lastIndex && !IsSqlKeywordPart(_line[currentIndex]); currentIndex++) { }
            return new Token(TokenType.ExecStatementText, startIndex, currentIndex - 1, _tokensLine);
        }
    }
}

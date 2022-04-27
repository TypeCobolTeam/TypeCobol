using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.Scanner
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
        private static bool IsSqlKeywordPart(char c)
        {
            return c == '-' || char.IsLetter(c);
        }

        public SqlScanner(string line, int startIndex, int lastIndex, TokensLine tokensLine,
            TypeCobolOptions compilerOptions)
            : base(line, startIndex, lastIndex, tokensLine, compilerOptions)
        {

        }

        public override Token GetTokenStartingFrom(int startIndex)
        {
            // Cannot read past end of line or before its beginning
            if (startIndex < 0 || startIndex > lastIndex)
            {
                return null;
            }

            // Start scanning at the given index
            currentIndex = startIndex;
            switch (line[startIndex])
            {
                case ' ':
                    //SpaceSeparator=1,
                    return ScanWhitespace(startIndex);
                case '*':
                    return ScanOneChar(startIndex, TokenType.MultiplyOperator);
                case ',':
                    return ScanOneChar(startIndex, TokenType.SQL_CommaSeparator);
                case '.':
                    return ScanOneChar(startIndex, TokenType.PeriodSeparator);
                case '(':
                    return ScanOneChar(startIndex, TokenType.LeftParenthesisSeparator);
                case ')':
                    return ScanOneChar(startIndex, TokenType.RightParenthesisSeparator);

            }

            if (IsSqlKeywordPart(line[currentIndex]))
            {
                //Consume all sql-keyword compatible chars
                for (; currentIndex <= lastIndex && IsSqlKeywordPart(line[currentIndex]); currentIndex++){}
                string tokenText = line.Substring(startIndex, currentIndex - startIndex);
                //Try to match keyword text
                var tokenType = TokenUtils.GetSqlKeywordTokenTypeFromTokenString(tokenText);
                switch (tokenType)
                {
                    case TokenType.SQL_COMMIT:
                    case TokenType.SQL_SELECT:
                    case TokenType.SQL_ALL:
                    case TokenType.SQL_DISTINCT:
                    case TokenType.UserDefinedWord:
                    case TokenType.SQL_FROM:
                    case TokenType.SQL_AS:
                        return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
                    //Unrecognized keyword (for now) return as ExecStatementText
                    default:
                        return new Token(TokenType.ExecStatementText, startIndex, currentIndex - 1, tokensLine);
                }
            }
            //Consume all sql-keyword incompatible chars
            for (; currentIndex <= lastIndex && !IsSqlKeywordPart(line[currentIndex]); currentIndex++){}
            return new Token(TokenType.ExecStatementText, startIndex, currentIndex - 1, tokensLine);
        }
    }
}


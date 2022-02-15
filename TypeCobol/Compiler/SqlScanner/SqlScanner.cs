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
        private readonly int _startIndex;
        private readonly int _lastIndex;
        private readonly TokensLine _tokensLine;
        private readonly TypeCobolOptions _compilerOptions;

        public int CurrentIndex { get; private set; }

        public SqlScanner(string line, int startIndex, int lastIndex, TokensLine tokensLine, TypeCobolOptions compilerOptions)
            : base()
        {
            _line = line;
            _startIndex = startIndex;
            _lastIndex = lastIndex;
            _tokensLine = tokensLine;
            _compilerOptions = compilerOptions;
            CurrentIndex = startIndex;
        }

        public override Token GetNextToken()
        {
            int curIndex = CurrentIndex;
            if (IsSqlKeywordPart(_line[curIndex]))
            {
                for (++curIndex; curIndex <= _lastIndex && IsSqlKeywordPart(_line[curIndex]); curIndex++) { }
                int curEndIndex = curIndex - 1;
                string tokenText = _line.Substring(_startIndex, curEndIndex - _startIndex + 1);

                // Try to match keyword text
                var tokenType = TokenUtils.GetTokenTypeFromTokenString(tokenText, true, CobolLanguageLevel.Cobol85);

                //TODO we need to distinguish between cobol words and SQL words !

                //So far this scanner only recognize 'COMMIT' keyword
                if (tokenType == TokenType.COMMIT)
                {
                    CurrentIndex = curIndex;
                    return new Token(TokenType.COMMIT, _startIndex, curEndIndex, _tokensLine);
                }
            }

            // Consume all chars as ExecStatementText
            CurrentIndex = _lastIndex + 1;
            return new Token(TokenType.ExecStatementText, _startIndex, _lastIndex, _tokensLine);
        }
    }
}

using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// A Sql Scanner related to a Scanner 
    /// </summary>
    public static class SqlScanner
    {
        /// <summary>
        /// True if the given char can be part of an SQL keyword.
        /// </summary>
        /// <param name="c">The character to test</param>
        /// <returns>True if yes, no otherwise</returns>
        public static bool IsSqlKeywordPart(char c)
        {
            return c == '-' || Char.IsLetter(c);
        }

        /// <summary>
        /// Scan a SQL keyword if one is recognized or a ExecStatementText token otherwise.
        /// </summary>
        /// <param name="startIndex">Start scanning index in the line</param>
        /// <param name="endIndex">End scanning index in the line</param>
        /// <param name="line">The line to be scanned</param>
        /// <param name="tokensLine">The associed TokensLine instance to be asociated with the token to create.</param>
        /// <param name="currentIndex">[out] the new end index</param>
        /// <returns>Either a SQL keyword token or a ExecStatementText token otherwise</returns>
        public static Token ScanSqlKeywordOrExecStatementText(int startIndex, int endIndex, string line, TokensLine tokensLine, out int currentIndex)
        {
            int curIndex = startIndex;
            if (IsSqlKeywordPart(line[curIndex]))
            {
                for (++curIndex; curIndex <= endIndex && IsSqlKeywordPart(line[curIndex]); curIndex++) { }
                int curEndIndex = curIndex - 1;
                string text = line.Substring(startIndex, curEndIndex - startIndex + 1);
                if (text.Equals("COMMIT", StringComparison.OrdinalIgnoreCase))
                {
                    currentIndex = curIndex;
                    return new Token(TokenType.COMMIT, startIndex, curEndIndex, tokensLine);
                }
            }
            // Consume all chars as ExecStatementText
            currentIndex = endIndex + 1;
            return new Token(TokenType.ExecStatementText, startIndex, endIndex, tokensLine);
        }
    }
}

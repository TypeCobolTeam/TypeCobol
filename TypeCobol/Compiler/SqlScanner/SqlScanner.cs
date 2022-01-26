using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// A Sql Scanner related to a Scanner 
    /// </summary>
    public static class SqlScanner
    {
        /// <summary>
        /// True if the given char can bepart of an SQL keyword.
        /// </summary>
        /// <param name="c">The character to test</param>
        /// <returns>True if yes, no otherwise</returns>
        public static bool IsSqlKeywordPart(char c)
        {
            return c == '-' || Char.IsLetter(c);
        }

        public static Token ScanSqlKeywordOrExecStatementText(ref int currentIndex, int startIndex, int endIndex, int lastIndex, string line, TokensLine tokensLine)
        {
            int curIndex = currentIndex;
            if (IsSqlKeywordPart(line[startIndex]))
            {
                for (; curIndex <= lastIndex && IsSqlKeywordPart(line[curIndex]); curIndex++) { }
                int curEndIndex = curIndex - 1;
                string text = line.Substring(startIndex, curEndIndex - startIndex + 1);
                if (text.Equals("COMMIT", StringComparison.OrdinalIgnoreCase)) {
                    currentIndex = curIndex;
                    return new Token(TokenType.COMMIT, startIndex, curEndIndex, tokensLine);
                }                    
            }
            // Consume all chars
            currentIndex = endIndex + 1;
            return new Token(TokenType.ExecStatementText, startIndex, endIndex, tokensLine);
        }
    }
}

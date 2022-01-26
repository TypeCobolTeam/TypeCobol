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
        public static Token ScanSqlKeywordOrExecStatementText(ref int currentIndex, int startIndex, int endIndex, int lastIndex, string line, TokensLine tokensLine)
        {
            int curIndex = currentIndex;
            if (SqlChar.IsSqlKeywordPart(line[startIndex]))
            {
                for (; curIndex <= lastIndex && SqlChar.IsSqlKeywordPart(line[curIndex]); curIndex++) { }
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

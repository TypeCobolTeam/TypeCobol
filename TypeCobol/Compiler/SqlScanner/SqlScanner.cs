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
    public class SqlScanner
    {
        public static bool IsSqlKeyword(ref int currentIndex, int startIndex, int lastIndex, string line, TokensLine tokensLine, out Token sqlToken)
        {
            sqlToken = null;
            int curIndex = currentIndex;
            if (SqlChar.IsSqlKeywordPart(line[startIndex]))
            {
                for (; curIndex <= lastIndex && SqlChar.IsSqlKeywordPart(line[curIndex]); curIndex++) { }
                int endIndex = curIndex - 1;
                string text = line.Substring(startIndex, endIndex- startIndex + 1);
                if (text.Equals("COMMIT", StringComparison.CurrentCultureIgnoreCase)) {
                    currentIndex = curIndex;
                    sqlToken = new Token(TokenType.COMMIT, startIndex, endIndex, tokensLine);
                    return true;
                }                    
            }
            return false;
        }
    }
}

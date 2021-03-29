using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// Check the possible role of Sql characters in source text
    /// </summary>
    public static class SqlChar
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
    }
}

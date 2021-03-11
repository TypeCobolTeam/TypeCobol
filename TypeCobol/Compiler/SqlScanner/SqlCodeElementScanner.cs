using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.SqlCodeElements;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// Class that implements the SQL Code Element Scanner
    /// </summary>
    public class SqlCodeElementScanner : TUVienna.CS_CUP.Runtime.Scanner
    {
        private int index = -1;
        /// <summary>
        /// The EOF symbol
        /// </summary>
        public static TUVienna.CS_CUP.Runtime.Symbol EOF => new TUVienna.CS_CUP.Runtime.Symbol(0, null);
        public IList<CodeElement> SqlCodeElements { get; private set; }
        public SqlCodeElementScanner(IList<CodeElement> sqlCodeElements)
        {
            SqlCodeElements = sqlCodeElements;
        }
        public Symbol next_token()
        {
            if (++index < SqlCodeElements.Count)
            {
                CodeElement ce = SqlCodeElements[index];
                TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol((int)ce.Type
                    - (int)SqlCodeElementType.FirstSqlCodeElementType + 2, ce);//+2 to skip EOF and error symbols
                return symbol;
            }
            else
            {
                return EOF;
            }
        }
    }
}

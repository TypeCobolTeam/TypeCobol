using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// Class that implements the SQL Code Element Scanner
    /// </summary>
    public class SqlCodeElementScanner : TUVienna.CS_CUP.Runtime.Scanner
    {
        private int index = -1;
        public IList<CodeElement> SqlCodeElements { get; private set; }
        public SqlCodeElementScanner(IList<CodeElement> sqlCodeElements)
        {

        }
        public Symbol next_token()
        {
            throw new NotImplementedException();
        }
    }
}

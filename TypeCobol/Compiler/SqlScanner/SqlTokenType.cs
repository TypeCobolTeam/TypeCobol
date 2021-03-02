using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.SqlScanner
{

    /// <summary>
    /// https://ronsavage.github.io/SQL/sql-2003-2.bnf.html
    /// BNF Grammar for ISO/IEC 9075-2:2003 - Database Language SQL (SQL-2003) SQL/Foundation 
    /// </summary>
    public enum SqlTokenType
    {

        /* keywords */
        EXEC = TokenType.LastTokenType,
        SQL,
        END,
        END_EXEC,
        COMMIT,
    }
}

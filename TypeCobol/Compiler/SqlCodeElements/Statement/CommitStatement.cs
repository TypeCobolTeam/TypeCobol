using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.SqlScanner;

namespace TypeCobol.Compiler.SqlCodeElements.Statement
{
    /// <summary>
    /// SQL COMMIT Statement Code Element.
    /// </summary>
    public class CommitStatement : SqlStatementElement
    {
        public CommitStatement(SqlToken commit) : base(SqlCodeElementType.CommitStatement, SqlStatementType.CommitStatement)
        {
            base.ConsumedTokens = new List<Token>() { commit };
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.SqlCodeElements
{
    /// <summary>
    /// List all types of Sql executable statements
    /// </summary>
    public enum SqlStatementType
    {
        CommitStatement = StatementType.LastStatementType,
    }
}

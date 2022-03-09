using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{/// <summary>
    /// SQL SELECT Statement Code Element.
    /// </summary>
    public class SelectStatement : SqlStatementElement
    {
        public SelectStatement() : base(CodeElementType.SelectStatement, StatementType.SelectStatement)
        {
        }
    }
}

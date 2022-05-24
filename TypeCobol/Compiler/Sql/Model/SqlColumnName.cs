using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class SqlColumnName : SqlExpression
    {
        public SqlColumnName(SqlSymbol symbol)
        {
            this.Symbol = symbol;
        }

        private SqlSymbol Symbol { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Symbol), Symbol, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(Symbol);
        }
    }
}

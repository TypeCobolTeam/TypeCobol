using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class TableViewCorrelationName : SqlObject
    {
        private SymbolReference _tableViewOrCorrelation;

        public TableViewCorrelationName(SymbolReference tableViewOrCorrelation)
        {
            _tableViewOrCorrelation = tableViewOrCorrelation;
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

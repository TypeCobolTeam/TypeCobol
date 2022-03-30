using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public abstract class TableReference
    {
        protected TableViewCorrelationName TableRef {get; set;}
    }

    public class SingleTableReference : TableReference
    {
        private CorrelationClause CorrelationClause {get;}

        public SingleTableReference(TableViewCorrelationName tableRef, CorrelationClause correlation)
        {
            this.TableRef = tableRef;
            this.CorrelationClause = correlation;
        }
    }

    public class CorrelationClause
    {
        private SymbolReference CorrelationName {get;}
        private List<SymbolReference> NewColumnNames {get;}

        public CorrelationClause(SymbolReference correlationName, List<SymbolReference> newColumnNames)
        {
            CorrelationName = correlationName;
            NewColumnNames = newColumnNames;
        }

    }
}

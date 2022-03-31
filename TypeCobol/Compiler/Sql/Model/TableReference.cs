using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    
    public class SingleTableReference 
    {
        public TableViewCorrelationName TableOrViewName { get; }
        public CorrelationClause CorrelationClause {get;}

        public SingleTableReference(TableViewCorrelationName tableOrViewName, CorrelationClause correlation)
        {
            this.CorrelationClause = correlation;
            TableOrViewName = tableOrViewName;
        }
    }

    public class CorrelationClause
    {
        public SymbolReference CorrelationName {get;}
        public List<SymbolReference> NewColumnNames {get;}

        public CorrelationClause(SymbolReference correlationName, List<SymbolReference> newColumnNames)
        {
            CorrelationName = correlationName;
            NewColumnNames = newColumnNames;
        }

    }
}

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    
    public class SingleTableReference : SqlObject
    {
        public TableViewCorrelationName TableOrViewName { get; }
        public CorrelationClause CorrelationClause {get;}

        public SingleTableReference(TableViewCorrelationName tableOrViewName, CorrelationClause correlation)
        {
            this.CorrelationClause = correlation;
            TableOrViewName = tableOrViewName;
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }

    public class CorrelationClause : SqlObject
    {
        public SymbolReference CorrelationName {get;}
        public List<SymbolReference> NewColumnNames {get;}

        public CorrelationClause(SymbolReference correlationName, List<SymbolReference> newColumnNames)
        {
            CorrelationName = correlationName;
            NewColumnNames = newColumnNames;
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

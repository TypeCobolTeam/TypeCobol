using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;
namespace TypeCobol.Compiler.Sql.Model
{
    public class SingleTableReference : SqlObject
    {
        public TableViewCorrelationName TableOrViewName { get; }
        public CorrelationClause CorrelationClause { get; }

        public SingleTableReference(TableViewCorrelationName tableOrViewName, CorrelationClause correlation)
        {
            this.CorrelationClause = correlation;
            TableOrViewName = tableOrViewName;
        }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(TableOrViewName), TableOrViewName, indentLevel);
            DumpProperty(output, nameof(CorrelationClause), CorrelationClause, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(TableOrViewName, CorrelationClause); 
        }
    }

    public class CorrelationClause : SqlObject
    {
        public SymbolReference CorrelationName {get;}
        public List<SqlColumnName> NewColumnNames {get;}

        public CorrelationClause(SymbolReference correlationName, List<SqlColumnName> newColumnNames)
        {
            CorrelationName = correlationName;
            NewColumnNames = newColumnNames;
        }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(CorrelationName), CorrelationName, indentLevel);
            DumpProperty(output, nameof(NewColumnNames), NewColumnNames, indentLevel);
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

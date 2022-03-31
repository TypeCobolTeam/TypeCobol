using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class TableViewCorrelationName : SqlObject
    {
        private readonly SymbolReference _tableViewOrCorrelation;

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

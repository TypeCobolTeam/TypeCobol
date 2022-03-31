using System.IO;

namespace TypeCobol.Compiler.Sql.Model
{
    public class SubSelect : SqlObject
    {
        public SubSelect(SelectClause selectClause, FromClause fromClause)
        {
            SelectClause = selectClause;
            FromClause = fromClause;
        }

        public SelectClause SelectClause { get; }

        public FromClause FromClause { get; }

        //TODO
        //WhereClause
        //GroupByClause
        //HavingClause
        //OrderByClause
        //OffsetClause
        //FetchClause
        public override void Dump(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(SelectClause), SelectClause, indentLevel);
            DumpProperty(output, nameof(FromClause), FromClause, indentLevel);
            
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(SelectClause, FromClause);
        }
    }
}

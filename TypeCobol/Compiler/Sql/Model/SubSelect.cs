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
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            if (this.SelectClause != null)
            {
                output.WriteLine("SelectClause");
                this.SelectClause.Dump(output,indentLevel+1);
            }
            if ( this.FromClause!=null)
            {
                output.WriteLine("FromClause");
                this.FromClause.Dump(output,indentLevel+1);
            }
            
        }
        
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(SelectClause, FromClause);
        }
    }
}

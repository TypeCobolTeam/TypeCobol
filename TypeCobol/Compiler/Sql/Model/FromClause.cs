namespace TypeCobol.Compiler.Sql.Model
{
    public class FromClause : SqlObject
    {
        //TODO
        //list of table references

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

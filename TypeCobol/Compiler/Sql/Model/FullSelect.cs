namespace TypeCobol.Compiler.Sql.Model
{
    public class FullSelect : SqlObject
    {
        public FullSelect(SubSelect subSelect)
        {
            SubSelect = subSelect;
        }

        public FullSelect(FullSelect subQuery)
        {
            SubQuery = subQuery;
        }

        public SubSelect SubSelect { get; }

        public FullSelect SubQuery { get; }

        //TODO
        //ValuesClause
        //SetOperations
        //OrderByClause
        //OffsetClause
        //FetchClause
    }
}

using System.IO;

namespace TypeCobol.Compiler.Sql.Model
{
    /// <summary>
    /// See documentation <see cref="https://www.ibm.com/docs/en/db2-for-zos/12?topic=queries-fullselect"/>  
    /// </summary>
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

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(SubSelect), SubSelect, indentLevel);
            DumpProperty(output, nameof(SubQuery), SubQuery, indentLevel);
        }

        //TODO
        //ValuesClause
        //SetOperations
        //OrderByClause
        //OffsetClause
        //FetchClause

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(SubSelect, SubQuery);
        }
    }
}

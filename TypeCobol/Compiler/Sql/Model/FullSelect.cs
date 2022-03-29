using System;
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

        public override void Dump(TextWriter output, int indentLevel)
        {
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine("FullSelect");
            if (this.SubQuery != null)
            { 
                output.WriteLine("SubQuery");
                this.SubQuery.Dump(output,indentLevel+1);
            }
            if (this.SubSelect != null)
            {
                this.SubSelect.Dump(output,indentLevel+1);
            }
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

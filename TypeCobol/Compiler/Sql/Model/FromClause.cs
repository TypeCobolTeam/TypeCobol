using System.IO;

namespace TypeCobol.Compiler.Sql.Model
{
    public class FromClause : SqlObject
    {
        //TODO
        //list of table references

        //TODO
        // To complete 
        public override void Dump(TextWriter output, int indentLevel)
        {
            output.Write(this);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

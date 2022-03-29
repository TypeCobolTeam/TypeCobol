using System.IO;

namespace TypeCobol.Compiler.Sql.Model
{
    public class FromClause : SqlObject
    {
        //TODO
        //list of table references

       
        public override void Dump(TextWriter output, int indentLevel)
        {
            output.Write("FromClause");
            //TODO
            // To finish writing the name of table reference 
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

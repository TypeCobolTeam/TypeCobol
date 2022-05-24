using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class TableViewCorrelationName : SqlObject
    {
        public SqlColumnName Name { get; }

        public TableViewCorrelationName(SqlColumnName name)
        {
            Name = name;
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Name), Name, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

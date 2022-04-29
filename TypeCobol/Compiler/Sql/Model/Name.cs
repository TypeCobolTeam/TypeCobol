using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class TableViewCorrelationName : SqlObject
    {
        public SymbolReference Name { get; }

        public TableViewCorrelationName(SymbolReference name)
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

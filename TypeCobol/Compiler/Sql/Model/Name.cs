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
        public override void Dump(TextWriter output, int indentLevel)
        {
            string indent = new string(' ', 2 * indentLevel);
            output.Write($"{indent}- {nameof(Name)} = ");
            output.WriteLine(Name != null ? Name.ToString() : "<NULL>");
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

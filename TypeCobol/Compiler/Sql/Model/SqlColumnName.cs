using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class SqlColumnName : SqlExpression
    {
        public SqlColumnName(SymbolReference symbol)
        {
            this.Symbol = symbol;
        }

        private SymbolReference Symbol { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Symbol), Symbol, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) ;
        }
    }
}

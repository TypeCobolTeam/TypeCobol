using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class TruncateClause : SqlObject
    {
        public TruncateClause(SymbolReference Name)
        {
            TableName = new IntrinsicStorageArea(Name);
        }

        private StorageArea TableName { get; }
        public bool IsDropStorage { get; set; }
        public bool IsReuseStorage { get; set; }
        public bool IsIgnoreDeleteTriggers { get; set; }
        public bool IsRestrictWhenDeleteTriggers { get; set; }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(TableName), TableName.SymbolReference, indentLevel);
        }
    }
}
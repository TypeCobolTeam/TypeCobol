using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class SavePointClause : SqlObject
    {
        public SymbolDefinition SavePointName { get; }
        public SavePointClause(SymbolDefinition savePointName)
        {
            SavePointName = savePointName;
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(SavePointName), SavePointName, indentLevel);
        }
    }
}
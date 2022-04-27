using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class FromClause : SqlObject
    {
        public List<SingleTableReference> TableReferences { get;}
       
        public FromClause(List<SingleTableReference> tableReferences)
        {
            this.TableReferences = tableReferences;
        }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(TableReferences), TableReferences, indentLevel);
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(TableReferences);
        }
    }
}

using System.Collections.Generic;
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
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

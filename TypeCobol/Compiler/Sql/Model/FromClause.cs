using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class FromClause : SqlObject
    {
        public List<SymbolReference> TableOrViewReferences { get;}
       
        public FromClause(List<SymbolReference> tableOrViewReferences)
        {
            this.TableOrViewReferences = tableOrViewReferences;
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

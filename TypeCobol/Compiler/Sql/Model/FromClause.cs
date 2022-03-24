using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class FromClause : SqlObject
    {
        public List<SymbolReference> ViewReferences { get;}
       
        public FromClause(List<SymbolReference> viewReferences)
        {
            this.ViewReferences = viewReferences;
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

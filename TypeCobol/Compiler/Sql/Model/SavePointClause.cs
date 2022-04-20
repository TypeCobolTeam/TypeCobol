using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Sql.Model
{
    public class SavePointClause : SqlObject
    {
        private String SavePointName { get; }
        public SavePointClause(string savePointName)
        {
            SavePointName = savePointName;
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}

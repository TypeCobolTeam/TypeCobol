using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.SqlCodeElements;
using TypeCobol.Compiler.SqlCodeElements.Statement;

namespace TypeCobol.Compiler.SqlNodes
{
    public interface Statement { }

    /// <summary>
    /// Sql COMMIT Node
    /// </summary>
    public class Commit : GenericNode<CommitStatement>, Statement
    {
        public Commit(CommitStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            if (astVisitor is IASTWithSqlVisitor astSqlVisitor) { 
                return astSqlVisitor.Visit(this);
            }
            return true;
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.SqlCodeElements.Statement;
using TypeCobol.Compiler.SqlNodes;

namespace TypeCobol.Compiler.SqlCodeElements
{
    /// <summary>
    /// Abstract Visitor that accept SQL COMMIT CodeElement and Node.
    /// </summary>
    public abstract class AbstractAstWithSqlVisitor : AbstractAstVisitor, IASTWithSqlVisitor
    {
        public virtual bool Visit([NotNull] CommitStatement acceptStatement)
        {
            return true;
        }

        public virtual bool Visit([NotNull] Commit commit)
        {
            return true;
        }
    }
}

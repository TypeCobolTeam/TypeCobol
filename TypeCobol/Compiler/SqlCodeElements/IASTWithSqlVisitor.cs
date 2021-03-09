using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.SqlCodeElements.Statement;
using TypeCobol.Compiler.SqlNodes;

namespace TypeCobol.Compiler.SqlCodeElements
{
    /// <summary>
    /// Visitor Interface that allow visiting Sql Coe Elements and Nodes
    /// </summary>
    public interface IASTWithSqlVisitor : IASTVisitor
    {
        bool Visit([NotNull] CommitStatement acceptStatement);

        bool Visit([NotNull] Commit commit);
    }
}

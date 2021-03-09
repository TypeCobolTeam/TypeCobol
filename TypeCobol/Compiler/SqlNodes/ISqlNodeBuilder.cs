using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.SqlCodeElements.Statement;

namespace TypeCobol.Compiler.SqlNodes
{
    /// <summary>
    /// Interface of a Sql Node Builder
    /// </summary>
    public interface ISqlNodeBuilder
    {
        /// <summary>
        /// Enter a Commit Statement Node
        /// </summary>
        /// <param name="commit">The correponding Commit Statement Code Element</param>
        void EnterCommit([NotNull]CommitStatement commit);
    }
}

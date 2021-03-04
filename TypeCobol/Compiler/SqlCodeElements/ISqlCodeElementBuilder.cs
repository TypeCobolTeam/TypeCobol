using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.SqlScanner;

namespace TypeCobol.Compiler.SqlCodeElements
{
    /// <summary>
    /// Interface of a Sql Code Element Builder
    /// </summary>
    public interface ISqlCodeElementBuilder
    {
        /// <summary>
        /// Create a Code Element that represents a SQL COMMIT Statement;
        /// </summary>
        /// <param name="commit">The COMMIT Token</param>
        /// <returns></returns>
        CodeElement CreateCommitStatement(SqlToken commit);
    }
}

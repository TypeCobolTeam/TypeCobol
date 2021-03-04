using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.SqlCodeElements;
using TypeCobol.Compiler.SqlCodeElements.Statement;
using TypeCobol.Compiler.SqlScanner;

namespace TypeCobol.Compiler.SqlParser
{
    /// <summary>
    /// Default Sql Parser Code Element Builder
    /// </summary>
    public class SqlCodeElementBuilder : ISqlCodeElementBuilder
    {
        public CodeElement CreateCommitStatement(SqlToken commit)
        {
            return new CommitStatement(commit);
        }
    }
}

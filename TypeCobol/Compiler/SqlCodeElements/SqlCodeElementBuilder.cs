using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.SqlCodeElements.Statement;

namespace TypeCobol.Compiler.SqlCodeElements
{
    public class SqlCodeElementBuilder : ISqlCodeElementBuilder
    {
        public CodeElement CreateCommitStatement(CodeElementsParser.CommitStatementContext context)
        {
            return new CommitStatement();
        }
    }
}

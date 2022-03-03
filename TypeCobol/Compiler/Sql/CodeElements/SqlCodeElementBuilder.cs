using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    public class SqlCodeElementBuilder
    {
        public CodeElement CreateCommitStatement(CodeElementsParser.CommitStatementContext context)
        {
            return new CommitStatement();
        }
    }
}

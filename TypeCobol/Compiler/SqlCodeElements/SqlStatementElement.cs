using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.SqlCodeElements
{
    /// <summary>
    /// Base class for all Sql statement CodeElements
    /// </summary>
    public abstract class SqlStatementElement : StatementElement
    {
        protected SqlStatementElement(CodeElementType codeElementType, StatementType statementType) 
            : base(codeElementType, statementType)
        {
        }
    }
};

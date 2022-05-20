using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Sql.CodeElements
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
        public override TextAreaType StartingArea => TextAreaType.Source;
    }
}

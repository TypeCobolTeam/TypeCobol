using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL ROLLBACK Statement Code Element.
    /// </summary>
    public class RollbackStatement : SqlStatementElement
    {
        public SavePointClause SavePointClause { get; }

            public RollbackStatement(SavePointClause savePointClause) : base(CodeElementType.RollbackStatement, StatementType.RollbackStatement)
            {
                this.SavePointClause = savePointClause;
            }

    }
}
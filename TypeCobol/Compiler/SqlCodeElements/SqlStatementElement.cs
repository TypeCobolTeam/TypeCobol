using System.Collections;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.SqlCodeElements
{
    /// <summary>
    /// Base class for all Sql statement CodeElements
    /// </summary>
    public abstract class SqlStatementElement : StatementElement
    {
        protected SqlStatementElement(SqlCodeElementType codeElementType, SqlStatementType statementType) 
            : base((CodeElementType)codeElementType, (StatementType)statementType)
        {
        }

        /// <summary>
        /// Type of executable Sql statement
        /// </summary>
        public SqlStatementType SqlStatementType => (SqlStatementType)base.StatementType;
    }
};

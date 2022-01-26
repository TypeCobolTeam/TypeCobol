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
        protected SqlStatementElement(CodeElementType codeElementType, StatementType statementType) 
            : base(codeElementType, statementType)
        {
        }

        /// <summary>
        /// Type of executable Sql statement
        /// </summary>
        public StatementType SqlStatementType => base.StatementType;
    }
};

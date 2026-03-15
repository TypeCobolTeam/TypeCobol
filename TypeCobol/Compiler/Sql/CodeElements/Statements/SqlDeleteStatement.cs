using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SqlDeleteStatement : SqlStatementElement
    {
        public string TableName { get; }
        public IList<HostVariableBinding> WhereBindings { get; }

        public SqlDeleteStatement(string tableName, IList<HostVariableBinding> whereBindings)
            : base(CodeElementType.SqlDeleteStatement, StatementType.SqlDeleteStatement)
        {
            TableName = tableName;
            WhereBindings = whereBindings ?? new List<HostVariableBinding>();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}

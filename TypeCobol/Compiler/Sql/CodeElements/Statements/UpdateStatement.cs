using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class UpdateStatement : SqlStatementElement
    {
        public string TableName { get; }
        public IList<HostVariableBinding> SetBindings { get; }
        public IList<HostVariableBinding> WhereBindings { get; }

        public UpdateStatement(string tableName, IList<HostVariableBinding> setBindings, IList<HostVariableBinding> whereBindings)
            : base(CodeElementType.UpdateStatement, StatementType.UpdateStatement)
        {
            TableName = tableName;
            SetBindings = setBindings ?? new List<HostVariableBinding>();
            WhereBindings = whereBindings ?? new List<HostVariableBinding>();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}

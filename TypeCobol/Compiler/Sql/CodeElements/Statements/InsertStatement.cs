using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class InsertStatement : SqlStatementElement
    {
        public string TableName { get; }
        public IList<string> Columns { get; }
        public IList<HostVariableBinding> HostVariables { get; }
        public bool HasSubselect { get; }

        public InsertStatement(string tableName, IList<string> columns, IList<HostVariableBinding> hostVariables, bool hasSubselect)
            : base(CodeElementType.InsertStatement, StatementType.InsertStatement)
        {
            TableName = tableName;
            Columns = columns;
            HostVariables = hostVariables ?? new List<HostVariableBinding>();
            HasSubselect = hasSubselect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}

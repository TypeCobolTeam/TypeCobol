using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SelectStatement : SqlStatementElement
    {
        public FullSelect FullSelect { get; }
        public IList<HostVariableBinding> IntoHostVariables { get; }
        public IList<HostVariableBinding> WhereHostVariables { get; }

        public SelectStatement(FullSelect fullSelect, IList<HostVariableBinding> intoHostVariables = null, IList<HostVariableBinding> whereHostVariables = null)
            : base(CodeElementType.SelectStatement, StatementType.SelectStatement)
        {
            FullSelect = fullSelect;
            IntoHostVariables = intoHostVariables ?? new List<HostVariableBinding>();
            WhereHostVariables = whereHostVariables ?? new List<HostVariableBinding>();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(FullSelect);
        }
    }
}

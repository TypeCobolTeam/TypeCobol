using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class GetDiagnosticsStatement : SqlStatementElement
    {
        public SyntaxProperty<bool> IsCurrent { get; }
        public SyntaxProperty<bool> IsStacked { get; }
        public GetDiagnosticInformation RequestedInformation { get; }

        public GetDiagnosticsStatement(SyntaxProperty<bool> isCurrent, SyntaxProperty<bool> isStacked, GetDiagnosticInformation requestedInformation) : base(CodeElementType.GetDiagnosticsStatement, StatementType.GetDiagnosticsStatement)
        {
            IsCurrent = isCurrent;
            IsStacked = isStacked;
            RequestedInformation = requestedInformation;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, IsCurrent, IsStacked)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(RequestedInformation);
        }
    }

}
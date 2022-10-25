using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    /// <summary>
    /// Sql GET DIAGNOSTICS Node
    /// </summary>
    public class GetDiagnostics : GenericNode<GetDiagnosticsStatement>, Statement
    {
        public GetDiagnostics(GetDiagnosticsStatement statement) : base(statement)
        {
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    /// <summary>
    /// Sql RELEASE SAVEPOINT Node
    /// </summary>
    public class ReleaseSavepoint : GenericNode<ReleaseSavepointStatement>, Statement
    {
        public ReleaseSavepoint(ReleaseSavepointStatement statement) : base(statement)
        {
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}

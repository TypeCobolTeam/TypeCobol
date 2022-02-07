using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.SqlCodeElements.Statement;

namespace TypeCobol.Compiler.SqlNodes
{
    /// <summary>
    /// Sql COMMIT Node
    /// </summary>
    public class Commit : GenericNode<CommitStatement>, Statement
    {
        public Commit(CommitStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}

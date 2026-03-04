using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    /// <summary>
    /// Sql CLOSE Node
    /// </summary>
    public class CloseCursor : GenericNode<SqlCloseStatement>, Statement
    {
        public CloseCursor(SqlCloseStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}

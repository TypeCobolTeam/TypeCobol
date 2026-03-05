using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class DeclareCursor : GenericNode<DeclareCursorStatement>, Statement
    {
        public DeclareCursor(DeclareCursorStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}

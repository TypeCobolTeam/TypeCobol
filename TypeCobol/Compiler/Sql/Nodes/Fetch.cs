using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class Fetch : GenericNode<FetchStatement>, Statement
    {
        public Fetch(FetchStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}

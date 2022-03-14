using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{ /// <summary>
    /// Sql COMMIT Node
    /// </summary>
    public class Select : GenericNode<SelectStatement>, Statement
    { 
        public Select(SelectStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
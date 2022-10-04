using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    /// <summary>
    /// Sql EXECUTE IMMEDIATE Node
    /// </summary>
    public class ExecuteImmediate : GenericNode<ExecuteImmediateStatement>, Statement
    {
        public ExecuteImmediate(ExecuteImmediateStatement statement) : base(statement)
        {
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
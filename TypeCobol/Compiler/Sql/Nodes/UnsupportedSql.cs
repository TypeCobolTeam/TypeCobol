using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    /// <summary>
    /// Sql UnsupportedSqlStatement Node
    /// </summary>
    public class UnsupportedSql : GenericNode<UnsupportedSqlStatement>, Statement
    {
        public UnsupportedSql(UnsupportedSqlStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}

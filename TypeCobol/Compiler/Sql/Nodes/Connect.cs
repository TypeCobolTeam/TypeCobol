using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    /// <summary>
    /// Sql CONNECT Node
    /// </summary>
    public class Connect : GenericNode<ConnectStatement>, Statement
    {
        public Connect(ConnectStatement statement) : base(statement)
        {
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

}

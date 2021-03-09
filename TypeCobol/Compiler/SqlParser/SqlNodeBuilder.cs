using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.SqlCodeElements.Statement;
using TypeCobol.Compiler.SqlNodes;

namespace TypeCobol.Compiler.SqlParser
{
    public class SqlNodeBuilder : ISqlNodeBuilder
    {
        /// <summary>
        /// The root Exec Node.
        /// </summary>
        public Exec Root { get; private set; }
        /// <summary>Branch currently in construction</summary>
        private Stack<Node> Branch;

        /// <summary>
        /// Current Parent node.
        /// </summary>
        public Node CurrentNode { get { return Branch.Peek(); } }
        public SqlNodeBuilder([NotNull] Exec exec)
        {
            this.Root = exec;
            this.Branch = new Stack<Node>();
            this.Branch.Push(Root);
        }

        /// <summary>Add a node as the Head's first child.</summary>
        /// <param name="child">Node to be added</param>
        public void Add(Node child)
        {
            CurrentNode.Add(child);            
        }
        /// <summary>Add a node as the Head's first child, the new child becomes current node.</summary>
        /// <param name="child">Node to add</param>
        public void Enter(Node child)
        {
            Add(child);
            Branch.Push(child);
        }
        /// <summary>Head's parent becomes the new Head.</summary>
        /// <returns>Exited node</returns>
        public void Exit()
        {
            Branch.Pop();// will throw InvalidOperationException if Branch is empty
        }

        /// <summary>
        /// Enter a Commit Statement Node
        /// </summary>
        /// <param name="commit">The correponding Commit Statement Code Element</param>   
        public void EnterCommit([NotNull] CommitStatement commit)
        {
            Commit node = new Commit(commit);
            Add(node);
        }
    }
}

using System.Collections.Generic;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.CodeModel
{
    public class SyntaxTree
    {
        /// <summary>Tree root</summary>
        public SourceFile Root { get; }

        public List<Node> Nodes { get; }

        /// <summary>Branch currently in construction</summary>
        private readonly Stack<Node> _branch;

        public SyntaxTree()
        {
            Root = new SourceFile();
            Nodes = new List<Node>() { Root };
            _branch = new Stack<Node>();
            _branch.Push(Root);
        }

        /// <returns>Tip of Branch.</returns>
        public Node CurrentNode => _branch.Peek();

        /// <summary>Add a node as the Head's first child.</summary>
        /// <param name="child">Node to add</param>
        public void Enter(Node child)
        {
            CurrentNode.Add(child);
            Nodes.Add(child);
            _branch.Push(child);
        }

        /// <summary>Head's parent becomes the new Head.</summary>
        /// <returns>Exited node</returns>
        public void Exit() => _branch.Pop(); // will throw InvalidOperationException if Branch is empty

        public override string ToString()
        {
            return Root.ToString();
        }
    }
}

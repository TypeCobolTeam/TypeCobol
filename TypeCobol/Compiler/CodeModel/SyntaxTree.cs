namespace TypeCobol.Compiler.CodeModel {

    using Antlr4.Runtime;
    using System;
    using System.Collections.Generic;
    using TypeCobol.Compiler.Nodes;
    using TypeCobol.Compiler.CodeElements;



    public class SyntaxTree {
        /// <summary>Tree root</summary>
        public SourceFile Root { get; private set; }
        /// <summary>Branch currently in construction</summary>
        private Stack<Node> Branch;

        public SyntaxTree(): this(new SourceFile()) { }
        public SyntaxTree(SourceFile root) {
			this.Root = root ?? new SourceFile();
			Branch = new Stack<Node>();
			Branch.Push(Root);
		}
        /// <returns>Tip of Branch.</returns>
        public Node CurrentNode { get { return Branch.Peek(); } }


        /// <summary>Add a node as the Head's first child.</summary>
        /// <param name="child">Node to add</param>
        /// <param name="context">Context child was created from</param>
        public void Enter(Node child) {
			CurrentNode.Add(child);
			Branch.Push(child);
		}
        /// <summary>Head's parent becomes the new Head.</summary>
        /// <returns>Exited node</returns>
        public void Exit() {
			Branch.Pop();// will throw InvalidOperationException if Branch is empty
		}



        public override string ToString() {
			return Root.ToString();
		}

    }
}

using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    public class Comment : Action
    {
        public string Group { get; private set; }
        internal Node Node;

        public Comment(Node node) { this.Node = node; }

        public void Execute() { comment(this.Node); }
        private void comment(Node node)
        {
            node.Comment = true;
            foreach (var child in node.Children) comment(child);
        }
    }
}

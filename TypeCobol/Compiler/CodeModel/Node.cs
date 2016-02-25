using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public class Node {
        private IList<Node> children_ = new List<Node>();
        public IList<Node> Children {
            get { return new System.Collections.ObjectModel.ReadOnlyCollection<Node>(children_); }
            private set { throw new System.InvalidOperationException(); }
        }
        public object CodeElement { get; internal set; }
        public Node Parent { get; internal set; }

        public Node(object e) { CodeElement = e; }

        internal void Add(Node child) {
            children_.Add(child);
            child.Parent = this;
        }
        internal void Remove() {
            Parent.children_.Remove(this);
            Parent = null;
        }

        public override string ToString() {
            if (CodeElement==null) return GetType().Name+"?";
            else return CodeElement.GetType().Name;
        }
    }
}

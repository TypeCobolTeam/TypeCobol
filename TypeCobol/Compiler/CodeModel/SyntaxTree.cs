using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    public class SyntaxTree
    {
        public Node Root { get; private set; }
        private Stack<Node> Branch = new Stack<Node>();

        public SyntaxTree(Node root) {
            this.Root = root;
            Attach(root);
        }

        public void Add(Node node) {
            Branch.Peek().Add(node);
        }
        public void Push(Node node) {
            Branch.Push(node);
        }
        public void Attach(Node node) {
            if (Branch.Count > 0) Add(node);
            Push(node);
        }
        public Node Detach() {
            return Branch.Pop();
        }
        public Node Delete() {
            Branch.Peek().Remove();
            return Detach();
        }

        public string ToString() {
            var str = new StringBuilder();
            ToString(Root, str, 0);
            return str.ToString();
        }
        private void ToString(Node node, StringBuilder str, int indent) {
            for(int c=1; c<indent; c++) str.Append("  ");
            str.Append("+ ");
            if (node.CodeElement==null) str.AppendLine(node.GetType().Name);
            else str.AppendLine(node.CodeElement.GetType().Name);
            foreach(var child in node.Children) ToString(child, str, indent+1);
        }
    }
}

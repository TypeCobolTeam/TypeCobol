using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    class AST
    {
        public Node Root { get; private set; }
        private Stack<Node> Branch = new Stack<Node>();

        public AST(Node root) {
            this.Root = root;
            Attach(root);
        }

        public void Attach(Node node) {
System.Console.WriteLine(">>> Attach "+node.GetType().Name+": "+(node.CodeElement!=null?node.CodeElement.GetType().Name:"?"));
            if (Branch.Count > 0)
                Branch.Peek().Add(node);
            Branch.Push(node);
        }
        public void Detach() {
            var node = Branch.Pop();
System.Console.WriteLine("<<< Detach "+node.GetType().Name+": "+(node.CodeElement!=null?node.CodeElement.GetType().Name:"?"));
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

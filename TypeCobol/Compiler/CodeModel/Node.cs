using System.Collections.Generic;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.CodeElements
{
	public class Node {
		private IList<Node> children_ = new List<Node>();
		public IList<Node> Children {
			get { return new System.Collections.ObjectModel.ReadOnlyCollection<Node>(children_); }
			private set { throw new System.InvalidOperationException(); }
		}
		public CodeElement CodeElement { get; internal set; }
		public Node Parent { get; internal set; }

		public Node(CodeElement e) { CodeElement = e; }

		internal void Add(Node child) {
			children_.Add(child);
			child.Parent = this;
		}
		internal void Remove() {
			Parent.children_.Remove(this);
			Parent = null;
		}

		/// <summary>Implementation of the GoF Visitor pattern.</summary>
		public void Accept(NodeVisitor visitor) {
			visitor.Visit(this);
		}

		public override string ToString() {
			if (CodeElement==null) return GetType().Name+"?";
			else return CodeElement.GetType().Name;
		}

		public CodeModel.SymbolTable SymbolTable { get; set; }
	}

	/// <summary>Implementation of the GoF Visitor pattern.</summary>
	public interface NodeVisitor {
		void Visit(Node node);
	}
}

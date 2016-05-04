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



		private static Dictionary<string,Attribute> Attributes;
		static Node() {
			Attributes = new Dictionary<string,Attribute>();
			Attributes["name"]  = new Named("NAME");
			Attributes["level"] = new Level("LEVEL");
			Attributes["type"]    = new Typed("TYPE");
			Attributes["typedef"] = new TypeDefinition("TYPEDEF");
			Attributes["sender.type"] = new SenderType("SENDER");
			Attributes["sender"] = new Sender("RECEIVER");
		}
		public string this[string attribute] {
			get {
				try { return Attributes[attribute].GetValue(CodeElement, SymbolTable); }
				catch(KeyNotFoundException ex) { return null; }
			}
		}
	}

	/// <summary>Implementation of the GoF Visitor pattern.</summary>
	public interface NodeVisitor {
		void Visit(Node node);
	}

	public interface Attribute {
		string GetValue(CodeElement ce, SymbolTable table);
	}
	internal abstract class NodeAttribute: Attribute {
		public string Key { get; private set; }
		public NodeAttribute(string key) { this.Key = key; }
		public abstract string GetValue(CodeElement ce, SymbolTable table);
	}

	internal class Named: NodeAttribute {
		public Named(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var data = ce as DataDescriptionEntry;
			if (data == null || data.Name == null) return null;
			return data.Name.Name;
		}
	}
	internal class Level: NodeAttribute {
		public Level(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var data = ce as DataDescriptionEntry;
			if (data == null) return null;
			return string.Format("{0:00}", data.LevelNumber);
		}
	}
	internal class Typed: NodeAttribute {
		public Typed(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var data = ce as DataDescriptionEntry;
			if (data == null || data.IsTypeDefinition) return null;
			if (!table.IsCustomType(data.DataType)) return null;
			return data.DataType.Name;
		}
	}
	internal class TypeDefinition: NodeAttribute {
		public TypeDefinition(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var data = ce as DataDescriptionEntry;
			if (data == null || !data.IsTypeDefinitionPart) return null;
			return data.DataType.Name;
		}
	}
	internal class SenderType: NodeAttribute {
		public SenderType(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var writer = ce as SymbolWriter;
			if (writer == null) return null;
			foreach(var symbol in writer.Symbols) {
				var type = symbol.Item1.Item2;
				if (type == null && symbol.Item1.Item1 != null) {
					var data = table.Get(symbol.Item1.Item1);
					if (data.Count > 0) type = data[0].DataType;
				}
				if (type == null) continue;
				return type.Name;
			}
			return null;
		}
	}
	internal class Sender: NodeAttribute {
		public Sender(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var writer = ce as SymbolWriter;
			if (writer == null) return null;
			foreach(var symbol in writer.Symbols) {
				var type = symbol.Item1.Item2;
//				var data = table.Get(symbol.Item2);
//				if (data.Count > 0) type = data[0].DataType;
//				return type.Name;
			}
			return null;
		}
	}
}

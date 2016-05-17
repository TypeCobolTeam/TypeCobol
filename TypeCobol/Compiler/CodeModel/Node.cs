using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;
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
			Attributes["typedef"] = new TypeDefined("TYPEDEF");
			Attributes["sender"] = new Sender("SENDER");
			Attributes["receiver"] = new Receiver("RECEIVER");
		}
		public string this[string attribute] {
			get {
				try {
					object value = CodeElement;
					foreach(var attr in attribute.Split(new char[] {'.'})) {
						value = Attributes[attr].GetValue(value, SymbolTable);
					}
					if (value == null) return null;
					return value.ToString();
				} catch(KeyNotFoundException ex) { return null; }
			}
		}
	}

	/// <summary>Implementation of the GoF Visitor pattern.</summary>
	public interface NodeVisitor {
		void Visit(Node node);
	}

	public interface Attribute {
		object GetValue(object o, SymbolTable table);
	}
	internal abstract class NodeAttribute: Attribute {
		public string Key { get; private set; }
		public NodeAttribute(string key) { this.Key = key; }
		public abstract object GetValue(object o, SymbolTable table);
	}

	internal class Named: NodeAttribute {
		public Named(string key): base(key) { }
		public override object GetValue(object o, SymbolTable table) {
			if (o is DataDescriptionEntry) {
				var data = o as DataDescriptionEntry;
				if (data.Name == null) return null;
				return data.Name.Name;
			} else
			if (o is Identifier) {
				return (o as Identifier).Name;
			} else
			return null;
		}
	}
	internal class Level: NodeAttribute {
		public Level(string key): base(key) { }
		public override object GetValue(object o, SymbolTable table) {
			var data = o as DataDescriptionEntry;
			if (data == null) return null;
			return string.Format("{0:00}", data.LevelNumber);
		}
	}
	internal class Typed: NodeAttribute {
		public Typed(string key): base(key) { }
		public override object GetValue(object o, SymbolTable table) {
			if (o is DataDescriptionEntry) {
				var data = o as DataDescriptionEntry;
				if (data.IsTypeDefinition) return null;
				if (!table.IsCustomType(data.DataType)) return null;
				return data.DataType;
			} else
			if (o is Literal) {
				var l = o as Literal;
				if (l.IsNumeric) return DataType.Numeric;
				if (l.IsBoolean) return DataType.Boolean;
				return DataType.Alphanumeric;
			} else
			return null;
		}
	}
	internal class TypeDefined: NodeAttribute {
		public TypeDefined(string key): base(key) { }
		public override object GetValue(object o, SymbolTable table) {
			var data = o as DataDescriptionEntry;
			if (data == null || !data.IsTypeDefinitionPart) return null;
			return data.DataType.Name;
		}
	}
	internal class Sender: NodeAttribute {
		public Sender(string key): base(key) { }
		public override object GetValue(object o, SymbolTable table) {
			var s = o as Sending;
			if (s == null) return null;
			return s.Expression;
		}
	}
	internal class Receiver: NodeAttribute {
		public Receiver(string key): base(key) { }
		public override object GetValue(object o, SymbolTable table) {
			var s = o as Receiving;
			if (s == null) return null;
			if (s.Expressions.Count < 1) return null;
			if (s.Expressions.Count == 1) return s.Expressions[0];
			return s.Expressions;
		}
	}
}

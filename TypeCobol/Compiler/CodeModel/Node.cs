using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Text;
using TypeCobol.Tools;

namespace TypeCobol.Compiler.CodeElements
{
	public class Node {
		private readonly IList<Node> children_ = new List<Node>();
		public IList<Node> Children {
			get { return children_; }
			private set { throw new System.InvalidOperationException(); }
		}
		public CodeElement CodeElement { get; internal set; }
		public Node Parent { get; internal set; }

		public Node(): this(null) { }
		public Node(CodeElement e) { CodeElement = e; }

		public void Add(Node child, int index = -1) {
			if (index < 0) children_.Add(child);
			else children_.Insert(index, child);
			child.Parent = this;
		}
		public void Remove() {
			Parent.children_.Remove(this);
			Parent = null;
		}
		public IList<Node> GetChildren(Type type) {
			var results = new List<Node>();
			foreach(var child in Children)
				if (child.CodeElement != null && Reflection.IsTypeOf(child.CodeElement.GetType(), type))
					results.Add(child);
			return results;
		}
		public Node GetParent(Type type) {
			var current = this;
			while (current != null)
				if (current.CodeElement != null && Reflection.IsTypeOf(current.CodeElement.GetType(), type))
					return current;
				else current = current.Parent;
			return null;
		}



		/// <summary>Implementation of the GoF Visitor pattern.</summary>
		public void Accept(NodeVisitor visitor) {
			visitor.Visit(this);
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			Dump(str, 0, this);
			return str.ToString();
		}

		private void Dump(System.Text.StringBuilder str, int level, Node node) {
			for(int c=0; c<level; c++) str.Append("  ");
			str.AppendLine(node.CodeElement==null?"?":node.CodeElement.GetType().Name);
			foreach(var child in node.Children) Dump(str, level+1, child);
		}

		public CodeModel.SymbolTable SymbolTable { get; set; }



		public virtual IEnumerable<ITextLine> Lines {
			get {
				var lines = new List<ITextLine>();
				if (CodeElement == null) return lines;
				foreach(var token in CodeElement.ConsumedTokens)
					if (!lines.Contains(token.TokensLine))
						lines.Add(token.TokensLine);
				return lines;
			}
		}
		public string ID {
			get {
				if (CodeElement is ProgramIdentification) return ((ProgramIdentification)CodeElement).ProgramName.Name;
				if (CodeElement is DataDivisionHeader) return "data-division";
				if (CodeElement is FileSectionHeader) return "file";
				if (CodeElement is LinkageSectionHeader) return "linkage";
				if (CodeElement is LocalStorageSectionHeader) return "local-storage";
				if (CodeElement is WorkingStorageSectionHeader) return "working-storage";
				if (CodeElement is DataDescriptionEntry) return ((DataDescriptionEntry)CodeElement).QualifiedName.ToString();
				if (CodeElement is EnvironmentDivisionHeader) return "environment-division";
				if (CodeElement is ProcedureDivisionHeader) return "procedure-division";
				if (CodeElement is ParagraphHeader) return ((ParagraphHeader)CodeElement).ParagraphName.Name;
				return null;
			}
		}
		public string URI {
			get {
				if (ID == null) return null;
				string puri = Parent == null?null:Parent.URI;
				if (puri == null) return ID;
				return puri+'.'+ID;
			}
		}
		public Node Root {
			get {
				var current = this;
				while (current.Parent != null) current = current.Parent;
				return current;
			}
		}
		public Node Get(string uri) {
			if (URI != null && URI.EndsWith(uri)) return this;
			foreach(var child in Children) {
				var found = child.Get(uri);
				if (found != null) return found;
			}
			return null;
		}



		private static Dictionary<string,Attribute> Attributes;
		static Node() {
			Attributes = new Dictionary<string,Attribute>();
			Attributes["name"]  = new Named("NAME");
			Attributes["level"] = new Level("LEVEL");
			Attributes["type"]    = new Typed("TYPE");
			Attributes["typedef"] = new TypeDefined("TYPEDEF");
			Attributes["sender"] = new Sender("SENDER");
			Attributes["receiver"] = new Receiver("RECEIVER");
			Attributes["functions"] = new UsesFunctions("FUNCTIONS");
			Attributes["function"] = new UsesFunctions("FUNCTION", true);
			Attributes["function-name"] = new UsesFunctions("FUNCTION", true, true);
		}
		public object this[string attribute] {
			get {
				try {
					object value = CodeElement;
					foreach(var attr in attribute.Split('.')) {
						value = Attributes[attr].GetValue(value, SymbolTable);
					}
					return value;
				} catch(KeyNotFoundException ex) { return null; }
			}
		}





		public static int CountAllChildren(Node node) {
			int count = node.Children.Count;
			foreach(var child in node.Children)
				count += CountAllChildren(child);
			return count;
		}

		public bool? Comment = null;
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
	internal class UsesFunctions: NodeAttribute
	{
	    private bool ReturnFirstFunctionOnly = false;
        private bool ReturnFunctionName = false;

        public UsesFunctions(string key, bool returnFirstFunctionOnly = false, bool returnFunctionName = false) : base(key)
	    {
	        this.ReturnFirstFunctionOnly = returnFirstFunctionOnly;
            this.ReturnFunctionName = returnFunctionName;
	    }
		public override object GetValue(object o, SymbolTable table) {
            var s = o as IdentifierUser;
			if (s == null) return null;
			var functions = new List<Function>();
			foreach(var id in s.Identifiers) {
				var reference = id as FunctionReference;
				if (reference == null) continue;
                var declaration = table.GetFunction(reference.Name);
				if (declaration == null) continue; // undefined symbol, not our job
                functions.Add(CreateFrom(reference, declaration));
			}
			if (functions.Count < 1) return null;
		    if (ReturnFirstFunctionOnly)
		    {
		        if (ReturnFunctionName)
		            return functions[0].Name;
		        else
		            return functions[0];
		    }
            //TODO support list of functions name
            return functions;
		}

		private Function CreateFrom(FunctionReference reference, Function declaration) {
			var parameters = new List<Parameter>();
			for(int c = 0; c < declaration.InputParameters.Count; c++) {
				var declared = declaration.InputParameters[c];
				string value = "SPACE";
				bool byReference = false;
				Parameter merged;
				try {
					var referenced = reference.Parameters[c];
					value = referenced.Value.ToString();
					byReference = referenced.Value is Identifier;
				} catch(System.ArgumentOutOfRangeException) { }
				merged = new CallParameter(value, byReference);
				merged.Type = declared.Type;
				merged.Length = declared.Length;
				merged.IsCustom = declared.IsCustom;
				parameters.Add(merged);
			}
			return new Function(declaration.QualifiedName, parameters, declaration.OutputParameters);
		}
	}
}

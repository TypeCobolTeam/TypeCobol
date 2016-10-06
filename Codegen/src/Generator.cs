using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen {

	public class Generator: NodeVisitor {

		private readonly IReadOnlyList<ICobolTextLine> Input;
		private readonly TextWriter Writer;
		private readonly List<Skeleton> Skeletons;

		private List<Action> Actions = null;


		public Generator(TextWriter destination, IReadOnlyList<ICobolTextLine> source, List<Skeleton> skeletons) {
			Input = source;
			Writer = destination;
			Skeletons = skeletons ?? new List<Skeleton>();
		}

		/// <summary>Generates code</summary>
		/// <param name="tree">Root of a syntax tree</param>
		/// <param name="table">Table of symbols</param>
		/// <param name="columns">Columns layout</param>
		public void Generate(Root tree, SymbolTable table, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
			Actions = new List<Action>();
			// STEP 1: modify tree to adapt it to destination language
			tree.Accept(this);
			var groups = new List<string>();
			foreach (var action in Actions) {
				if (action.Group != null && groups.Contains(action.Group)) continue;
				action.Execute();
				if (action.Group != null) groups.Add(action.Group);
			}
//			Console.WriteLine(tree.Root.ToString());

			// STEP 2: convert tree to destination language code
			var converter = new TreeToCode(Input, columns);
			tree.Accept(converter);
			converter.WriteInputLinesUntilEnd();
			Writer.Write(converter.Output.ToString());
			Writer.Flush();
//			Console.WriteLine(converter.Output.ToString());
		}

		public void Visit(Node node) {
			var actions = GetActions(node);
			Actions.AddRange(actions);
			foreach(var child in node.Children) child.Accept(this);
		}

		private ICollection<Action> GetActions(Node node) {
			var actions = new List<Action>();
			var skeleton = GetActiveSkeleton(node);
			if (skeleton != null) {
				var properties = GetProperties(node, skeleton.Properties);
				foreach(var pattern in skeleton) {
					var action = GetAction(node, properties, pattern);
					if (action != null) actions.Add(action);
				}
			}
			return actions;
		}

		private Skeleton GetActiveSkeleton(Node node) {
			foreach(var skeleton in Skeletons) {
				bool active = false;
				foreach(var condition in skeleton.Conditions) {
					active = active || condition.Verify(node); // OR
				}
				if (active) return skeleton;//TODO: what if more than 1 skel activates?
			}
			return null;
		}

		private Dictionary<string,object> GetProperties(Node node, IEnumerable<string> properties) {
			var result = new Dictionary<string,object>();
			var errors = new System.Text.StringBuilder();
			foreach(var pname in properties) {
				var property = node[pname];
				if (property != null) result[pname] = property;
				else errors.Append(pname).Append(", ");
			}
			if (errors.Length > 0) {
				errors.Length -= 2;
				errors.Insert(0, "Undefined properties for "+node.GetType().Name+": ");
				errors.Append(". (line:\"").Append(node.CodeElement.InputStream).Append("\")");
				throw new System.ArgumentException(errors.ToString());
			}
			return result;
		}

		private Action GetAction(Node source, Dictionary<string,object> properties, Pattern pattern) {
			int? index;
			var destination = GetLocation(source, pattern.Location, out index);
			if ("create".Equals(pattern.Action)) {
				return new Create(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter, index);
			}
			if ("replace".Equals(pattern.Action)) {
				return new Replace(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter);
			}
			if ("comment".Equals(pattern.Action)) {
				return new Comment(destination);
			}
			if ("expand".Equals(pattern.Action)) {
				return new Expand(source, destination, pattern.Location);
			}
			if ("erase".Equals(pattern.Action)) {
				return new Erase(destination, pattern.Template);
			}
			System.Console.WriteLine("Unknown action: \""+pattern.Action+"\"");
			return null;
		}

		private Node GetLocation(Node node, string location, out int? index) {
			index = null;
			if (location.EndsWith(".begin")) {
				location = location.Substring(0, location.Length - ".begin".Length);
				index = 0;
			} else
			if (location.EndsWith(".end")) {
				location = location.Substring(0, location.Length - ".end".Length);
			}

			if (location == null || location.ToLower().Equals("node")) return node;
			var root = node;
			while(root.Parent != null) root = root.Parent;

			var result = root.Get(location);
			if (result != null) return result;
			throw new System.ArgumentException("Undefined URI: "+location);
		}
	}

	public interface Action {
		string Group { get; }
		/// <summary>Modifies AST.</summary>
		void Execute();
	}

	public class Create: Action {
		public string Group { get; private set; }
		internal Node Parent;
		private Node Child;
	    private int? position;

		public Create(Node parent, string template, Dictionary<string,object> variables, string group, string delimiter, int? position) {
			this.Parent = parent;
			if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
			var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
			this.Child = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver);
		    this.position = position;
		}

		public void Execute() {
			Parent.Add(Child, (position ?? -1));
		}
	}

	public class Replace: Action {
		public string Group { get; private set; }
		internal Node Old;
		private Node New;

		public Replace(Node node, string template, Dictionary<string,object> variables, string group, string delimiter) {
			this.Old = node;
			if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
			var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
			this.New = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver);
		}

		public void Execute() {
			var parent = Old.Parent;
			int index = parent.IndexOf(Old);
		    Old.Comment = true;
			parent.Add(New, index+1);
		}
	}

	public class Comment: Action {
		public string Group { get; private set; }
		internal Node Node;

		public Comment(Node node) { this.Node = node; }

		public void Execute() { comment(this.Node); }
		private void comment(Node node) {
			node.Comment = true;
			foreach(var child in node.Children) comment(child);
		}
	}

	public class Expand: Action {
		public string Group { get; private set; }
		internal Node Source;
		internal Node Destination;
		internal string DestinationURI;
		private Dictionary<Type,Type> Generators = new Dictionary<Type,Type> {
				{ typeof(DataDescriptionEntry), typeof(TypedDataNode) },
				{ typeof(FunctionDeclarationHeader), typeof(Codegen.Nodes.FunctionDeclaration) },
			};

		public Expand(Node source, Node destination, string destinationURI) {
			this.Source = source;
			this.Destination = destination;
			this.DestinationURI = destinationURI;
		}

		public void Execute() {
			// retrieve data
			int index;
			if (DestinationURI.EndsWith(".end")) index = this.Destination.Parent.Children.Count-1;
			else index = this.Destination.Parent.IndexOf(this.Destination);

			if (index > -1) {
				var typegen = GetGeneratedNode(this.Source.CodeElement.GetType());
				var nodegen = (Node)Activator.CreateInstance(typegen, this.Source);
				this.Destination.Parent.Add(nodegen, index+1);
			}
			// comment out original "line" (=~ non expanded node)
			this.Source.Comment = true;
			this.Source.RemoveAllChildren();
		}

		private Type GetGeneratedNode(Type type) {
			try { return Generators[type]; }
			catch(KeyNotFoundException) { throw new ArgumentException("Unknown type "+type); }
		}
	}

	public class Erase: Action {
		public string Group { get; private set; }
		internal Node Node;
		private IEnumerable<string> Words;

		public Erase(Node node, string word) {
			this.Node = node;
			this.Words = new List<string> { word };
		}

		public void Execute() {
			var solver = new Skeletons.Templates.Eraser(Node.CodeElement.SourceText, this.Words);
			bool somethingToDo = solver.Run();
			if (!somethingToDo) return;

			// retrieve data
			int index = this.Node.Parent.IndexOf(this.Node);
			if (index > -1) {
				var nodegen = new GeneratedNode(solver);
				this.Node.Parent.Add(nodegen, index+1);
			}
			// comment out original "line" (=~ non expanded node)
			this.Node.Comment = true;
			this.Node.Clear();

		}
	}
}

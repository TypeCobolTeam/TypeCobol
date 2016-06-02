using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen {

	public class Generator: NodeVisitor {

		private readonly IReadOnlyList<ICobolTextLine> Input;
		private readonly List<ICobolTextLine> Output;
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
		public void Generate(Node tree, SymbolTable table, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
			Actions = new List<Action>();
			// STEP 1: modify tree to adapt it to destination language
			tree.Accept(this);
			var groups = new List<string>();
			foreach (var action in Actions) {
				if (action.Group != null && groups.Contains(action.Group)) continue;
				action.Execute();
				if (action.Group != null) groups.Add(action.Group);
			}

			// STEP 2: convert tree to destination language code
			var converter = new TreeToCode(Input, columns);
			tree.Accept(converter);
			Writer.Write(converter.Output.ToString());
			Writer.Flush();

			Console.WriteLine(converter.Output.ToString());
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
			    if (node[pname] != null)
			    {
			        result[pname] = node[pname];
			    }
			    else
			    {
			        errors.Append(node.GetType()).Append("[").Append(pname).Append("]").Append(',').Append(node.CodeElement.InputStream);
			        
			    }
			}
			if (errors.Length > 0) {
				errors.Length -= 1;
				throw new System.ArgumentException("Undefined properties for node: "+errors);
			}
			return result;
		}

		private Action GetAction(Node source, Dictionary<string,object> properties, Pattern pattern) {
			var destination = GetLocation(source, pattern.Location);
			if ("create".Equals(pattern.Action)) {
				return new Create(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter, pattern.Position);
			}
			if ("replace".Equals(pattern.Action)) {
				return new Replace(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter);
			}
			if ("comment".Equals(pattern.Action)) {
				return new Comment(destination);
			}
			if ("expand".Equals(pattern.Action)) {
				return new Expand(destination);
			}
			System.Console.WriteLine("Unknown action: \""+pattern.Action+"\"");
			return null;
		}

		private Node GetLocation(Node node, string location) {
			if (location == null || location.ToLower().Equals("node")) return node;
			Node root = node;
			while(root.Parent != null) root = root.Parent;
			var result = root.Get(location);
			if (result != null) return result;
			throw new System.ArgumentException("Undefined URI: "+location);
		}



		private string CreateGeneratedText(Node node, Pattern pattern) {
			var variables = CreateVariables(pattern.Variables, node);
			string generated = Config.Cheetah.Replace(pattern.Template, variables, pattern.Delimiter);
			if (pattern.Trim) generated = generated.Trim();
			if (pattern.Indent) {
				string indent = CreateIndent(node.CodeElement as DataDescriptionEntry);
				var lines = generated.Split('\n');
				var str = new System.Text.StringBuilder();
				foreach(string line in lines) str.Append(indent+line);
				generated = str.ToString();
			}
			return generated;
		}

		private Dictionary<string,string> CreateVariables(Dictionary<string,string> variables, Node node) {
			string delimiter = "$";
			var results = new Dictionary<string,string>();
			foreach(var key in variables.Keys) {
				string value = variables[key];
				if (value.StartsWith(delimiter)) //remove starting delimiter
					value = node[value.Trim().Substring(delimiter.Length)].ToString();
				results[key] = value;
			}
			return results;
		}

		private string CreateIndent(DataDescriptionEntry data) {
			var indent = "       ";
			if (data != null) {
				for(int c=0; c<data.Generation; c++) indent += "  ";
			}
			return indent;
		}
	}

	public interface Action {
		string Group { get; }
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
		    if (position != null)
		    {
                Console.WriteLine("insert new node at position " + position);
                Parent.Children.Insert((int) position, Child);
            }
		    else
		    {
		        Parent.Children.Add(Child);
		    }
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
			int index = parent.Children.IndexOf(Old);
		    Old.Comment = true;
			parent.Children.Insert(index+1, New);
            
		}
	}

	public class Comment: Action {
		public string Group { get; private set; }
		internal Node Node;

		public Comment(Node node) {
			this.Node = node;
		}

		public void Execute() {
			comment(this.Node);
		}
		private static void comment(Node node) {
			node.Comment = true;
			foreach(var child in node.Children) comment(child);
		}
	}

	public class Expand: Action {
		public string Group { get; private set; }
		internal Node Node;

		public Expand(Node node) {
			this.Node = node;
		}

		public void Execute() {
			// comment out original "line" (=~ non expanded node)
			this.Node.Comment = true;
			this.Node.Children.Clear();
var ce = this.Node.CodeElement;
var d = ce as DataDescriptionEntry;
System.Console.WriteLine("Execute("+(ce==null?"?":(d==null?ce.GetType().Name:d.QualifiedName.ToString()))+"), children="+this.Node.Children.Count);
//			new CommentNode(this.Node).Execute();
			// retrieve data
			int index = this.Node.Parent.Children.IndexOf(this.Node);
			this.Node.Parent.Children.Insert(index+1, new TypedDataNode(this.Node));
		}
	}
}

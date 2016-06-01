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


		public Generator(TextWriter destination, IReadOnlyList<ICobolTextLine> source, Tools.CodeElementDiagnostics converter, List<Skeleton> skeletons) {
			Input = source;
			Output = new List<ICobolTextLine>();
			Output.AddRange(source);
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

			var treeToCode = new TreeToCode(Input, columns);
			// STEP 2: convert tree to destination language code
			tree.Accept(treeToCode);
            
			Writer.Write(treeToCode.Output.ToString());
            Writer.Flush();
		    
            Console.WriteLine(treeToCode.Output.ToString());
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

        /// <summary>
        /// 
        /// </summary>
        /// <param name="source"></param>
        /// <param name="properties"></param>
        /// <param name="pattern"></param>
        /// <param name="position"></param>
        /// <returns></returns>
		private Action GetAction(Node source, Dictionary<string,object> properties, Pattern pattern) {
			var destination = GetLocation(source, pattern.Location);
			if ("create".Equals(pattern.Action)) {
				return new CreateNode(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter, pattern.Position);
			}
			if ("replace".Equals(pattern.Action)) {
				return new ReplaceNode(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter);
			}
			if ("comment".Equals(pattern.Action)) {
				return new CommentNode(destination);
			}
			if ("expand".Equals(pattern.Action)) {
				return new ExpandNode(destination);
			}
//					if ("comment".Equals(pattern.Action)) return new Comment(Output);
//					if ("delete" .Equals(pattern.Action)) return new Delete(Output);
//					if ("expand" .Equals(pattern.Action)) return new GenerateCustomTypedDataDescription(Output, Table);
//					if ("replace".Equals(pattern.Action)) return new Replace(Output, CreateGeneratedText(node, pattern));
//					return new Write(Output);// no peculiar codegen --> write as is
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

	public class CreateNode: Action {
		public string Group { get; private set; }
		internal Node Parent;
		private Node Child;
	    private int? position;

		public CreateNode(Node parent, string template, Dictionary<string,object> variables, string group, string delimiter, int? position) {
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

	public class ReplaceNode: Action {
		public string Group { get; private set; }
		internal Node Old;
		private Node New;

		public ReplaceNode(Node node, string template, Dictionary<string,object> variables, string group, string delimiter) {
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

	public class CommentNode: Action {
		public string Group { get; private set; }
		internal Node Node;

		public CommentNode(Node node) {
			this.Node = node;
		}

		public void Execute() {
			Comment(this.Node);
		}
		private static void Comment(Node node) {
			node.Comment = true;
			foreach(var child in node.Children) Comment(child);
		}
	}

	public class ExpandNode: Action {
		public string Group { get; private set; }
		internal Node Node;

		public ExpandNode(Node node) {
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




	public interface OldAction {
		bool Execute(Node node, List<ITokensLine> lines);
	}

	public class Write: OldAction {
		protected List<ICobolTextLine> output;
		protected CodeLineFactory factory;
		public Write(List<ICobolTextLine> output) {
			this.output = output;
			this.factory = new CodeLineFactory();
		}
		public virtual bool Execute(Node node, List<ITokensLine> lines) {
			// nothing to do
			return true;
		}

		protected int IndexOf(ITokensLine line) {
			int index = 0;
			foreach(var l in output)
				if (l == line) return index;
				else index++;
			return -1;
		}

		protected int Replace(ITokensLine oldline, ICobolTextLine newline) {
			int index = IndexOf(oldline);
			if (index > -1) {
				output.Remove(oldline);
				output.Insert(index, newline);
			}
			return index;
		}
	}

	public class Delete: Write {
		public Delete(List<ICobolTextLine> output): base(output) { }
		public override bool Execute(Node node, List<ITokensLine> lines) {
			foreach(var line in lines) output.Remove(line);
			return true;
		}
	}

	public class Comment: Write {
		public Comment(List<ICobolTextLine> output): base(output) { }
		public override bool Execute(Node node, List<ITokensLine> lines) {
			foreach(var line in lines) {
				Replace(line, factory.CreateCommentedLine(line));
			}
			return true;
		}
	}

	public class Replace: Write {
		private readonly string text;

		public Replace(List<ICobolTextLine> output, string text)
			: base(output) {
			this.text = text;
		}

		public override bool Execute(Node node, List<ITokensLine> lines) {
			var data = node.CodeElement as DataDescriptionEntry;
			foreach(var line in lines) {
				var index = IndexOf(line);
				output.Remove(line);
				output.Insert(index++, factory.CreateInsertedLine(factory.CreateCommentedLine(line)));
				output.Insert(index,   factory.CreateInsertedLine(text));
			}
			return false;
		}
	}

	public class GenerateCustomTypedDataDescription: Write {
		protected SymbolTable table;
		public GenerateCustomTypedDataDescription(List<ICobolTextLine> output, SymbolTable table)
			: base(output) {
			this.table = table;
		}
		public override bool Execute(Node node, List<ITokensLine> lines) {
			var data = node.CodeElement as DataDescriptionEntry;
			int index = -1;
			foreach(var line in lines) {
				index = IndexOf(line);
				output.Remove(line);
				output.Insert(index++, factory.CreateInsertedLine(factory.CreateCommentedLine(line)));
				output.Insert(index, factory.CreateCustomTypedData(line, data.DataType.Name));
			}
			var type = table.GetCustomType(data.DataType.Name);
			insertChildren(type, ref index, data.LevelNumber+1, data.Generation);
			return false;
		}

		private void insertChildren(TypeDefinition type, ref int line, int level, int generation) {
			foreach(var child in type.Subordinates) {
				bool isCustomTypeToo = !child.IsTypeDefinition && table.IsCustomType(child.DataType);
				ICobolTextLine inserted = factory.CreateDataDefinition(child, level, generation, isCustomTypeToo);
				output.Insert(++line, inserted);
				if (isCustomTypeToo) {
					var indent = "            ";
					for(int c=0; c<generation; c++) indent += "  ";
					output.Insert(++line, factory.CreateCommentedLine(factory.CreateInsertedLine(indent+child.Name+" TYPE "+child.DataType.Name)));
					insertChildren(table.GetCustomType(child.DataType.Name), ref line, level+1, generation+1);
				}
			}
		}
	}

	public class CodeLineFactory {
		public ICobolTextLine CreateEmptyLine() {
			return new EmptyLine();
		}
		public ICobolTextLine CreateCommentedLine(ITextLine line) {
			return new CommentedLine(line);
		}
		public ICobolTextLine CreateInsertedLine(ITextLine line) {
			return new InsertedLine(line.Text);
		}
		public ICobolTextLine CreateInsertedLine(string line) {
			return new InsertedLine(line);
		}

		public ICobolTextLine CreateCustomTypedData(ITokensLine line, string typename) {
			return new EditedLine(line, new RemoveCustomTypeDeclaration(typename));
		}

		internal ICobolTextLine CreateDataDefinition(DataDescriptionEntry data, int level, int generation, bool isCustomType) {
			var line = new System.Text.StringBuilder("           ");
			for(int c=0; c<generation; c++) line.Append("  ");
			line.Append(level.ToString("00")).Append(' ').Append(data.Name.Name);
			if (!isCustomType) line.Append(" PIC ").Append(data.Picture);
			line.Append('.');
			return new InsertedLine(line.ToString());
		}
	}

	public class LineForCodegen: ICobolTextLine {
		protected ITextLine OriginalLine;
		public LineForCodegen(ITextLine line) {
			this.OriginalLine = line;
		}

		/// <summary>That's the only thing we need. The rest of what's implemented throws InvalidOperationException.</summary>
		public virtual string Text { get { return OriginalLine.Text; } }

        /// from ICobolTextLine
		public CobolTextLineType Type { get { throw new System.InvalidOperationException(); } }
		public TextArea SequenceNumber { get { throw new System.InvalidOperationException(); } }
		public string SequenceNumberText { get { throw new System.InvalidOperationException(); } }
		public TextArea Indicator { get { throw new System.InvalidOperationException(); } }
		public char IndicatorChar { get { throw new System.InvalidOperationException(); } }
		public TextArea Source { get { throw new System.InvalidOperationException(); } }
		public string SourceText { get { throw new System.InvalidOperationException(); } }
		public TextArea Comment { get { throw new System.InvalidOperationException(); } }
		public string CommentText { get { throw new System.InvalidOperationException(); } }
		public Compiler.Concurrency.CompilationStep CompilationStep {
			get { throw new System.InvalidOperationException(); }
			set { throw new System.InvalidOperationException(); }
		}
		public bool CanStillBeUpdatedBy(Compiler.Concurrency.CompilationStep updatingStep) { throw new System.InvalidOperationException(); }
		// from ITextLine
		public string TextSegment(int startIndex, int endIndexInclusive) { throw new System.InvalidOperationException(); }
		public int Length { get { throw new System.InvalidOperationException(); } }
		public bool IsReadOnly { get { throw new System.InvalidOperationException(); } }
		public int InitialLineIndex { get { throw new System.InvalidOperationException(); } }
		public object LineTrackingReferenceInSourceDocument { get { throw new System.InvalidOperationException(); } }
	}

	public class EmptyLine: LineForCodegen {
		public EmptyLine(): base(null) { }
		public override string Text { get { return ""; } }
	}

	public class CommentedLine: LineForCodegen {
		public CommentedLine(ITextLine line): base(line) { }
		public override string Text {
			get {
				string edited = OriginalLine.Text;
				edited = edited.Remove(6, 1);
				edited = edited.Insert(6, "*");
				return edited;
			}
		}
	}

	public class InsertedLine: LineForCodegen {
		private readonly string text;
		public InsertedLine(string text)
			: base(null) {
			this.text = text;
		}
		public override string Text { get { return text; } }
	}

	public class EditedLine: LineForCodegen {
		protected Edit edit;
		public EditedLine(ITextLine line, Edit edit)
			: base(line) {
			this.edit = edit;
		}
		public override string Text {
			get { return edit.Edit(OriginalLine.Text); }
		}
	}

	public interface Edit {
		string Edit(string span);
	}
	public class RemoveTypeDefinition: Edit {
		public string Edit(string span) {
			int start = span.IndexOf(" TYPEDEF");
			if (start == -1 ) return span; // nothing to do
			int end = start + " TYPEDEF".Length;
			for(start=start-1; span[start]==' '; start--) ;
			start++;
			span = span.Replace(span.Substring(start,end-start), "");
			return span;
		}
	}
	public class RemoveCustomTypeDeclaration: Edit {
		private readonly string typename;
		public RemoveCustomTypeDeclaration(string typename) {
			this.typename = typename;
		}
		public string Edit(string span) {
			int start = span.IndexOf("TYPE ");
			if (start == -1 ) return span; // nothing to do
			int end = span.IndexOf(" "+typename, start) ;
			if (end != -1) end += typename.Length +1;
			else end = start + "TYPE ".Length;
			for(start=start-1; span[start]==' '; start--) ;
			start++;
			span = span.Replace(span.Substring(start,end-start), "");
			return span;
		}
	}
}

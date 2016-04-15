using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen {

	public class Generator: NodeVisitor {

		private IReadOnlyList<ICobolTextLine> Input;
		private TextWriter Output;
		private Tools.CodeElementDiagnostics Converter;

		/// <summary>Table of symbols</summary>
		private SymbolTable Table;
		/// <summary>
		/// Input as a stack of sourcecode lines.
		/// Lines are unstacked while Output is written to.
		/// </summary>
		private Stack<ITextLine> Lines;
		/// <summary>Sourcecode line currently being written.</summary>
		private ITextLine CurrentLine;
		/// <summary>Last offset in CurrentLine having been written.</summary>
		private int LastOffset;

		public Generator(TextWriter destination, IReadOnlyList<ICobolTextLine> source, Tools.CodeElementDiagnostics converter) {
			Input = source;
			Output = destination;
			Converter = converter;
		}

		/// <summary>Generates code</summary>
		/// <param name="tree">Root of a syntax tree</param>
		/// <param name="table">Table of symbols</param>
		/// <param name="columns">Columns layout</param>
		public void Generate(Node tree, SymbolTable table, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
			Table = table;
			// we create a stack of text lines, so each tree node could choose to:
			// - write generated code without unstacking --> insert code
			// - unstack and write without modification  --> copy code
			// - unstack, modify and then write          --> alter code
			// - unstack and do nothing with it          --> delete code
			// each node could then pass the stack or not to its children
			Lines = new System.Collections.Generic.Stack<ITextLine>(Input);// Lines are reversed...
			Lines = new System.Collections.Generic.Stack<ITextLine>(Lines);// ... so, put them back in order
//			while(Lines.Count > 0) Output.WriteLine(Lines.Pop().Text);

			CurrentLine = Lines.Pop();
			LastOffset = 0;
			tree.Accept(this);

			//tree.GenerateCode(stream, Lines);
			// this stack thingie is far from perfect however, as a node far away in the tree traversal
			// (eg. in PROCEDURE DIVISION) could want to insert code between some lines that are already
			// written/unstacked (eg. in DATA DIVISION) ...
			// => we should rather do all the text modifications, and only then write stuff
		}

		public void Visit(Node node) {
			//TODO retrieve action corresponding to node
			Action action = new WriteBeforeCodeElement(Output, Converter);
			action.execute(node, Lines, ref CurrentLine, ref LastOffset);
			action = GetAction(node);
			bool visitChildren = action.execute(node, Lines, ref CurrentLine, ref LastOffset);
			if (visitChildren) foreach(var child in node.Children) child.Accept(this);
		}

		private Action GetAction(Node node) {
			var data = node.CodeElement as DataDescriptionEntry;
			if (data != null) {
				if (!data.IsTypeDefinition && Table.IsCustomType(data.DataType))//TODO tcTypeClauseExt (ie. contains "TYPE")
					return new WriteCustomTypeDefinition(Output, Converter, Table);
			}
			return new WriteCodeElement(Output, Converter);
		}
	}

	public interface Action {
		/// <param name="node">Data to write</param>
		/// <param name="lines">Stack of lines (the line on top of the stack follows immediately the current line)</param>
		/// <param name="current">Line currently being written</param>
		/// <param name="offset">Index in the current line of the last written character</param>
		/// <returns>Should node.Children be visited?</returns>
		bool execute(Node node, Stack<ITextLine> lines, ref ITextLine current, ref int offset);
	}

	public abstract class WriteAction: Action {
		protected TextWriter writer;
		protected Tools.CodeElementDiagnostics converter;
		public WriteAction(TextWriter writer, Tools.CodeElementDiagnostics converter) {
			this.writer = writer;
			this.converter = converter;
		}

		public abstract bool execute(Node node, Stack<ITextLine> lines, ref ITextLine current, ref int offset);

		protected void newline(Stack<ITextLine> lines, ref ITextLine current, ref int offset) {
			writer.WriteLine();
			current = (lines.Count > 0 ? lines.Pop() : null);
			offset = 0;
		}
		protected int GetLine(ITextLine line) {
			return line.InitialLineIndex+1; //TODO: real index using converter
		}
	}

	public class DoNotWrite: WriteAction {
		public DoNotWrite(TextWriter writer, Tools.CodeElementDiagnostics converter) : base(writer, converter) { }
		public override bool execute(Node node, Stack<ITextLine> lines, ref ITextLine current, ref int offset) {
			var ce = node.CodeElement as CodeElement;
			if (ce == null) return true;
			var range = converter.GetRange(ce);
			var last = GetDeepestLastChildNotNullCodeElement(node);
			if (last != ce) {
				var endrange = converter.GetRange(last);
				range.End.Line = endrange.End.Line;
				range.End.Character = endrange.End.Character;
			}

			return false;
		}

		private CodeElement GetDeepestLastChildNotNullCodeElement(Node node) {
			Node res = node;
			foreach(var child in node.Children)
				if (child.CodeElement != null && child.CodeElement is CodeElement) res = child;
			if (res != node) return GetDeepestLastChildNotNullCodeElement(res);
			return node.CodeElement as CodeElement;
		}
	}

	public class WriteCustomTypeDefinition: WriteAction {
		private SymbolTable table;
		public WriteCustomTypeDefinition(TextWriter writer, Tools.CodeElementDiagnostics converter, SymbolTable Table)
			: base(writer, converter) {
			this.table = Table;
		}
		public override bool execute(Node node, Stack<ITextLine> lines, ref ITextLine current, ref int offset) {
			var ce = node.CodeElement as DataDescriptionEntry;
			if (ce == null) return true;
			var range = converter.GetRange(ce);
			string span;
			// write multi-line CodeElement first lines
			for(int l = range.Start.Line; l < range.End.Line; l++) {
				span = current.TextSegment(offset, current.Length-1);
				span = cleanupTypeDeclaration(span, ce.DataType.Name);
//System.Console.WriteLine("> CE_HEAD_LINE: \""+span+"\"");
				writer.WriteLine(span);
				newline(lines, ref current, ref offset);
			}
			// write CodeElement last line (=first line if it's a single-line CodeElement)
//System.Console.WriteLine(">>> [os:"+offset+", range:"+range+"] ("+GetLine(current)+")");
//if (range.End.Line != GetLine(current)) System.Console.WriteLine(">>> ce:\""+ce.Text+"\"  vs  \""+current.Text+"\"");
			span = current.TextSegment(offset, range.End.Character);
			span = cleanupTypeDeclaration(span, ce.DataType.Name);
System.Console.WriteLine("> CE_TAIL_LINE: \""+span+"\":"+ce.DataType.Name+" {"+node.Children.Count+"}");
			writer.Write(span);
			offset = range.End.Character+1;

			return false;
		}

		private string cleanupTypeDeclaration(string span, string type) {
			int start = span.IndexOf("TYPE ");
			if (start == -1 ) return span; //wtf?
			int end = span.IndexOf(" "+type, start);
			if (end != -1) end += type.Length +1;
			else end = start + "TYPE ".Length;
			for(start=start-1; span[start]==' '; start--) ;
			start++;
			span = span.Replace(span.Substring(start,end-start), "");
			return span;
		}
	}

	public class WriteBeforeCodeElement: WriteAction {
		public WriteBeforeCodeElement(TextWriter writer, Tools.CodeElementDiagnostics converter)
		: base(writer, converter) { }

		public override bool execute(Node node, Stack<ITextLine> lines, ref ITextLine current, ref int offset) {
			var ce = node.CodeElement as CodeElement;
			if (ce == null) return true;
			var range = converter.GetRange(ce);
//System.Console.WriteLine(">>> execute( range:"+range+", lines:"+lines.Count+", current: \""+current.Text+"\", os:"+offset+")");
			string span;
			if (GetLine(current) < range.Start.Line) {
				// write end of "last current" line, then newline
				span = current.TextSegment(offset, current.Length-1);
//System.Console.WriteLine("> END_OF_LAST_LINE: \""+span+"\"");
				writer.Write(span);
				newline(lines, ref current, ref offset);
			}
			while(GetLine(current) < range.Start.Line) {
				// write lines before
//System.Console.WriteLine("> LINE_BEFORE: \""+current.Text+"\"");
				writer.Write(current.Text);
				newline(lines, ref current, ref offset);
			}
			if (offset < range.Start.Character) {
				// write start of line (before CodeElement)
				span = current.TextSegment(offset, range.Start.Character-1);
				writer.Write(span);
//System.Console.WriteLine("> START_OF_LINE: \""+span+"\"");
				offset = range.Start.Character;
			}
			return true;
		}
	}

	public class WriteCodeElement: WriteAction {
		public WriteCodeElement(TextWriter writer, Tools.CodeElementDiagnostics converter)
		: base(writer, converter) { }

		/// <summary>Writes all sourcecode corresponding to node.CodeElement.</summary>
		public override bool execute(Node node, Stack<ITextLine> lines, ref ITextLine current, ref int offset) {
			var ce = node.CodeElement as CodeElement;
			if (ce == null) return true;
			var range = converter.GetRange(ce);
			string span;
			// write multi-line CodeElement first lines
			for(int l = range.Start.Line; l < range.End.Line; l++) {
				span = current.TextSegment(offset, current.Length-1);
				writer.WriteLine(span);
				newline(lines, ref current, ref offset);
			}
			// write CodeElement last line (=first line if it's a single-line CodeElement)
			span = current.TextSegment(offset, range.End.Character);
			writer.Write(span);
			offset = range.End.Character+1;

			return true;
		}
	}
}

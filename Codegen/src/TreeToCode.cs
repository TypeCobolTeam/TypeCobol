namespace TypeCobol.Codegen {

	using System.Collections.Generic;
	using System.Text;
	using TypeCobol.Codegen.Nodes;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;



	public class TreeToCode: NodeVisitor {

		/// <summary>Input source code</summary>
		public readonly IList<ICobolTextLine> Input;
		/// <summary>Columns layout of the generated text</summary>
		private ColumnsLayout Layout;
		/// <summary>Generated code is written here</summary>
		public System.IO.StringWriter Output;
		/// <summary>Index in Input of the next line to write</summary>
		private int offset = 0;
		/// <summary>Last line written</summary>
		private ITextLine lastline = null;

		public TreeToCode(IEnumerable<ICobolTextLine> source = null, ColumnsLayout layout = ColumnsLayout.FreeTextFormat) {
			if (source == null) Input = new List<ICobolTextLine>();
			else Input = new List<ICobolTextLine>(source);
			Output = new System.IO.StringWriter();
			this.Layout = layout;
		}

		public void Visit<T>(Node<T> node) where T:CodeElement {
			bool doVisitChildren = Process(node);
			if (doVisitChildren) foreach(var child in node.GetChildren()) child.Accept(this);
		}

		private bool Process<T>(Node<T> node) where T:CodeElement {
			string text = "";
			var generated = node as Generated;
			foreach(var line in generated.Lines) {
				if (generated != null)
					// if we write generated code, we INSERT one line of code between Input lines;
					// thus, we must decrease offset as it'll be re-increased by Write(line) and
					// we don't want to fuck up next iteration
					offset--;
				else
					// before we copy an original line of code, we must still write non-source
					// lines (eg. comments or empty lines) so they are preserved in Output
					WriteInputLinesUpTo(line);
				Write(line, node.Comment);
			}
			return generated == null || !generated.IsLeaf;
		}

		/// <summary>
		/// Write all lines between the last written line (ie. Input[offset-1]) and a given line.
		/// If line is contained in Input but before offset, all remaining Input will be written.
		///	In other words: don't fall in this case.
		/// </summary>
		/// <param name="line"></param>
		/// <returns>Number of lines written during this method call.</returns>
		private int WriteInputLinesUpTo(ITextLine line) {
			if (!IsInInput(line)) return 0;
			int lines = 0;
			while (offset < Input.Count) {
				var l = Input[offset];
				if (l == line) break;
				if (ShouldCopy(l)) Write(l, null);
				// offset is normally increased by a call to Write,
				// so don't forget to do it to avoid infinite loop
				else offset++;
				lines++;
			}
			return lines;
		}
		/// <summary>
		/// Only copy from input to output lines that are comment or blank.
		/// Everything else is either:
		///  - COBOL source, written by original AST nodes
		///  - TypeCobol source, written by AST generated nodes
		///  - invalid lines (wtf?)
		/// Source lines are of type Source, Debug or Continuation in CobolTextLineType enum.
		/// </summary>
		/// <param name="line"></param>
		/// <returns></returns>
		private bool ShouldCopy(ICobolTextLine line) {
			return line.Type == CobolTextLineType.Comment || line.Type == CobolTextLineType.Blank;
		}
		/// <summary>Write input lines up to the end.</summary>
		public void Finalize() {
			while (offset < Input.Count)
				Write(Input[offset], null);
		}

		private bool IsInInput(ITextLine line) {
			int c = offset;
			while (c < Input.Count) {
				if (Input[c] == line) return true;
				c++;
			}
			return false;
		}

		/// <summary>
		/// Writes one line of Input as one or more lines in Output.
		///	A single line, once indented, can output as many lines, especially on 80 colons.
		///	The value of offset is increased once as part of the Write operation.
		/// </summary>
		/// <param name="line">Input[offset]</param>
		/// <param name="isComment">Must line be commented ?</param>
		private void Write(ITextLine line, bool? isComment) {
			if (line == lastline) return;
			int c = 0;
			foreach(var l in Indent(line, isComment)) {
				Output.WriteLine(l.Text);
				c++;
			}
			offset++;
			lastline = line;
		}

		private IEnumerable<ITextLine> Indent(ITextLine line, bool? isComment) {
			var results = new List<ITextLine>();
			var cobol = line as CobolTextLine;
			if (cobol != null) {
				if (Layout == ColumnsLayout.CobolReferenceFormat) {
					results.Add(SetComment(line, isComment));
				} else
				if (Layout == ColumnsLayout.FreeTextFormat) {
					results.Add(SetComment(new TextLineSnapshot(-1, cobol.SourceText ?? "", null), isComment));
				} else
					throw new System.NotImplementedException("Unsuported columns layout: "+Layout);
			} else {
				if (Layout == ColumnsLayout.CobolReferenceFormat) {
					var lines = CobolTextLine.Create(line.Text, Layout, line.InitialLineIndex);
					foreach(var l in lines) results.Add(SetComment(l, isComment));
				} else
				if (Layout == ColumnsLayout.FreeTextFormat) {
					results.Add(SetComment(line, isComment));
				} else
					throw new System.NotImplementedException("Unsuported columns layout: "+Layout);
			}
			if (results.Count < 1)
				throw new System.NotImplementedException("Unsuported ITextLine type: "+line.GetType());
			return results;
		}

		private static ITextLine SetComment(ITextLine line, bool? isComment) {
			if (isComment == true)
				return Comment(line);
			else
			if (isComment == false)
				return Uncomment(line);
			else // null
				return line;
		}
		private static ITextLine Comment(ITextLine line) {
			var cobol = line as CobolTextLine;
			if (cobol != null) {
				StringBuilder text = new StringBuilder(cobol.Text);
				text[6] = '*';
				var lines = CobolTextLine.Create("*"+cobol.SourceText, cobol.ColumnsLayout, cobol.InitialLineIndex);
				foreach(var l in lines) return l;// there's only one in the collection
				throw new System.NotImplementedException("I should have at least one item!");
			} else {
				return new TextLineSnapshot(line.InitialLineIndex, "*"+line.Text, null);
			}
		}
		private static ITextLine Uncomment(ITextLine line) {
			var cobol = line as CobolTextLine;
			if (cobol != null) {
				StringBuilder text = new StringBuilder(cobol.Text);
				text[6] = ' ';
				var lines = CobolTextLine.Create(text.ToString(), cobol.ColumnsLayout, cobol.InitialLineIndex);
				foreach(var l in lines) return l;// there's only one in the collection
				throw new System.NotImplementedException("I should have at least one item!");
			} else {
				StringBuilder text = new StringBuilder(line.Text);
				int index = line.Text.IndexOf('*');
				text[index] = ' ';
				return new TextLineSnapshot(line.InitialLineIndex, text.ToString(), null);
			}
		}
	}

}

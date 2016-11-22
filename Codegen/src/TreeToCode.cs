namespace TypeCobol.Codegen {

	using System;
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

		public void Visit(Node node) {
			bool doVisitChildren = Process(node);
			if (doVisitChildren) foreach(var child in node.Children) child.Accept(this);
		}

		private bool Process(Node node) {
            //TODO If Node inside a copy are not useful for step 1 of Generator, we can move this to step1.
		    if (node.IsInsideCopy()) {
		        return false;
		    }
			var generated = node as Generated;
			foreach(var line in node.Lines) {
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
System.Console.WriteLine("ShouldCopy(\""+line.Text+"\":"+line.GetType().Name+"): "+line.CompilationStep+","+line.Type);
			return line.Type == CobolTextLineType.Comment || line.Type == CobolTextLineType.Blank
			   || line.Type == CobolTextLineType.Debug // #267: Debug lines are copied "AS IS", even if they are invalid in COBOL85!
			   || (line.Type == CobolTextLineType.Source && line.SourceText.Trim().StartsWith("COPY"));
		}
		/// <summary>Write input lines up to the end.</summary>
		public void WriteInputLinesUntilEnd() {
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

System.Console.Write("\"Write(\""+line.Text+"\":"+line.GetType().Name+", "+(isComment==null?"?":isComment.ToString())+")");
			int c = 0;
			foreach(var l in ConvertLine(line, isComment)) {
				bool endsLine = true;
				if (line is TextLineSnapshot) endsLine = ((TextLineSnapshot)line).EndsLine;
				if (endsLine) Output.WriteLine(l.Text);
				else Output.Write(l.Text);
				c++;
			}
			offset++;
			lastline = line;
		}


////////////////////////////////////////
		private IEnumerable<ITextLine> ConvertLine(ITextLine line,bool? isComment) {
			if (line is CobolTextLine) return ConvertOriginalLine((CobolTextLine)line, isComment);
			if (line is TextLineSnapshot) return ConvertGeneratedLine((TextLineSnapshot)line, isComment);
			throw new ArgumentException("Unsupported line type: "+line.GetType()+" (\""+line.Text+"\"@"+line.InitialLineIndex+")");
		}

		private string previousIndent = null;
		private IEnumerable<ITextLine> ConvertOriginalLine(CobolTextLine line, bool? comment) {
System.Console.Write(" --- original");
//			if (Layout == line.ColumnsLayout) {
//				var lines = new List<ITextLine>();
//				lines.Add(line); //nothing to do
//				return lines;
//			} else {
				string indent, code;
				GetIndent(line.SourceText ?? "", out indent, out code);
System.Console.WriteLine(": \""+line.SequenceNumberText+"\" \'"+line.IndicatorChar+"\' \""+indent+"\"("+indent.Length+") \""+code+"\" \""+line.CommentText+"\"");
				previousIndent = indent;
				char indicator = line.IndicatorChar;
				if (comment != null && comment == true) indicator = '*';
				return CreateLines(Layout, line.InitialLineIndex, line.SequenceNumberText, indicator, indent, code, line.CommentText, true, true);
//			}
		}
		/// <summary>
		/// This method assumes two things
		///  * a TextLineSnapshot is ALWAYS a generated line
		///  * a generated line is NEVER a comment
		/// </summary>
		/// <param name="line"></param>
		/// <param name="comment"></param>
		/// <returns></returns>
		private IEnumerable<ITextLine> ConvertGeneratedLine(TextLineSnapshot line, bool? comment) {
System.Console.Write(" --- generated");
			string indent, code;
			GetIndent(line.Text ?? "", out indent, out code);
System.Console.Write(": \""+indent+"\"("+indent.Length+(previousIndent!=null && previousIndent.Length != indent.Length ? (">"+previousIndent.Length.ToString()):"")+") \""+code+"\"");
			if (previousIndent != null) indent = previousIndent + indent;
System.Console.WriteLine();
			var lines = new List<ITextLine>();
			bool starts = line.StartsLine;
			bool ends   = line.EndsLine;
			// if line parameter can be a comment, the way indicator, starts and end are set must be fixed
			char indicator = ' ';
			bool wasComment = code.StartsWith("*");
			if (wasComment) {
				indicator = '*';
				code = code.Substring(1);
			}
			if (comment != null) {
				if (comment == true) {
					indicator = '*';
					starts = true;
				} else {
					indicator = ' ';
				}
			}
			return CreateLines(Layout, line.InitialLineIndex, null, indicator, indent, code, null, starts, ends);
		}
		private void GetIndent(string text, out string indent, out string indented) {
			int begin = System.Array.FindIndex(text.ToCharArray(), x => !char.IsWhiteSpace(x));
			if (begin == -1) begin = 0;// text.Length == 0
			indent = text.Substring(0, begin);
			indented = text.Substring(begin, text.Length-begin);
		}


		private static IEnumerable<ITextLine> CreateLines(ColumnsLayout layout, int index,
				string sequence, char indicator, string indent, string code,string endOfLineComment,
				bool startsLine, bool endsLine) {
			var result = new List<ITextLine>();
			var lines = Split(code, 65);
			result.Add(new TextLineSnapshot(index, Format(layout, startsLine, sequence, indicator, indent, lines[0], endsLine, endOfLineComment), startsLine, endsLine || lines.Count > 1));
			if (indicator == null || indicator == ' ') indicator = '-';
			for(int i = 1; i < lines.Count; i++) {
				if (index > -1) index++;
				result.Add(new TextLineSnapshot(index, Format(layout, startsLine, sequence, indicator, indent, lines[i], endsLine, endOfLineComment), true, endsLine || ((i+1) < lines.Count)));
			}
			return result;
		}
		private static IList<string> Split(string line, int max) {
			var lines = new List<string>();
			if (line.Length < 1) lines.Add(line);
			else {
				for (int i = 0; i < line.Length; i += max)
					lines.Add(line.Substring(i, Math.Min(max, line.Length-i)));
			}
			return lines;
		}
		private static string Format(ColumnsLayout layout,
		                             bool startsLine, string sequence, char indicator, string indent,
		                             string code,
		                             bool endsLine, string endOfLineComment) {
			var line = new StringBuilder();
			if (startsLine) {
				if (layout == ColumnsLayout.CobolReferenceFormat) {
					if (sequence != null) {
						if (sequence.Length != 6) throw new ArgumentException("Invalid sequence \""+sequence+"\" (expected length:6)");
						else line.Append(sequence);
					} else line.Append("101010");
				}
				// if layout == ColumnsLayout.FreeTextFormat, drop sequence text
				line.Append(indicator);
				line.Append(indent);
			}
			line.Append(code);
			if (endsLine && endOfLineComment != null) line.Append(endOfLineComment);
			return line.ToString();
		}
////////////////////////////////////////


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
					bool startsLine = true;
					bool endsLine   = true;
					if (line is TextLineSnapshot) {
						var snapshot = (TextLineSnapshot)line;
						startsLine = snapshot.StartsLine;
						endsLine   = snapshot.EndsLine;
					}
					var lines = CobolTextLine.Create(line.Text, Layout, line.InitialLineIndex, startsLine,endsLine);
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

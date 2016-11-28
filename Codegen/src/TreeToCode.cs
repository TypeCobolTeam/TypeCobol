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
			return line.Type == CobolTextLineType.Comment || line.Type == CobolTextLineType.Blank
			   || line.Type == CobolTextLineType.Debug // #267: Debug lines are copied "AS IS", even if they are invalid in COBOL85!
			   || IsPreprocessingDirective(line);
		}
		private bool MultilinePreprocessorDirective = false;
		private string[] PD = { "COPY", "REPLACE", "SKIP", "EJECT", };
		private bool IsPreprocessingDirective(ICobolTextLine line) {
			if (line.Type != CobolTextLineType.Source) return false;
			if (line.CompilationStep != Compiler.Concurrency.CompilationStep.Text) return false;
			string code = line.SourceText.TrimEnd();
			if (MultilinePreprocessorDirective) {
				MultilinePreprocessorDirective = !code.EndsWith(".");
				return true;
			}
			code = code.TrimStart();
			foreach(var word in PD)
				if (code.StartsWith(word, StringComparison.InvariantCultureIgnoreCase)) {
					MultilinePreprocessorDirective = !code.EndsWith(".");
					return true;
				}
			return false;
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

		int CurrentLineLength = 0;
		/// <summary>
		/// Writes one line of Input as one or more lines in Output.
		///	A single line, once indented, can output as many lines, especially on 80 colons.
		///	The value of offset is increased once as part of the Write operation.
		/// </summary>
		/// <param name="line">Input[offset]</param>
		/// <param name="isComment">Must line be commented ?</param>
		private void Write(ITextLine line, bool? isComment) {
			if (line == lastline) return;
			foreach(var l in ConvertLine(line, isComment)) {
				bool endsLine = true;
				if (isComment == true) {
					// fix #365: newly commented nodes have their children removed.
					// so, consider any such cases as complete lines in case of
					// one of a commented node's children ends the node line
					endsLine = true;
				} else
				if (line is TextLineSnapshot) {
					endsLine = ((TextLineSnapshot)line).EndsLine;
				} else if (line is CobolPartialTextLine) endsLine = ((CobolPartialTextLine)line).EndsLine;
				if (endsLine) {
					Output.WriteLine(l.Text);
					CurrentLineLength = 0;
				} else {
					Output.Write(l.Text);
					CurrentLineLength += l.Text.Length;
				}
			}
			if (CurrentLineLength == 0) offset++;
			lastline = line;
		}

		private IEnumerable<ITextLine> ConvertLine(ITextLine line,bool? isComment) {
			if (line is CobolTextLine) return ConvertOriginalLine((CobolTextLine)line, isComment);
			if (line is TextLineSnapshot) return ConvertGeneratedLine((TextLineSnapshot)line, isComment);
			throw new ArgumentException("Unsupported line type: "+line.GetType()+" (\""+line.Text+"\"@"+line.InitialLineIndex+")");
		}

		private string previousIndent = null;
		private char previousIndicator = ' ';
		private IEnumerable<ITextLine> ConvertOriginalLine(CobolTextLine line, bool? comment) {
			char indicator = line.IndicatorChar;
			if (comment != null && comment == true) indicator = '*';
			string indent, code;
			bool starts, ends;
			if (line is CobolPartialTextLine) {
				var lihn = (CobolPartialTextLine)line;
				starts = lihn.StartsLine;
				ends   = lihn.EndsLine;
				indent = lihn.Indent;
				code   = lihn.SourceText;
			} else {
				starts = true;
				ends   = true;
				Tools.Strings.GetIndent(line.SourceText ?? "", out indent, out code);
			}
			previousIndicator = line.IndicatorChar;
			if (previousIndicator != '*') {
				previousIndent = indent;
			}
			return CreateLines(Layout, line.InitialLineIndex, starts,line.SequenceNumberText,indicator,indent, code, ends,CurrentLineLength,line.CommentText);
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
			string indent, code;
			Tools.Strings.GetIndent(line.Text ?? "", out indent, out code);
			if (previousIndent != null && previousIndicator != '*') indent = previousIndent + indent;
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
			return CreateLines(Layout, line.InitialLineIndex, starts,null,indicator,indent, code, ends,CurrentLineLength,null);
		}


		private static IEnumerable<ITextLine> CreateLines(ColumnsLayout layout, int index,
				bool startsLine, string sequence, char indicator, string indent, string code,
				bool endsLine, int linelength, string endOfLineComment) {
			var result = new List<ITextLine>();
			var lines = Split(code, 65);
			bool starts = startsLine;
			bool ends = endsLine || lines.Count > 1;
			result.Add(new TextLineSnapshot(index, Format(layout, starts,sequence,indicator,indent, lines[0], ends,linelength,endOfLineComment), starts, ends));
			if (indicator == null || indicator == ' ') indicator = '-';
			for(int i = 1; i < lines.Count; i++) {
				if (index > -1) index++;
				starts = true;
				ends = endsLine || ((i+1) < lines.Count);
				result.Add(new TextLineSnapshot(index, Format(layout, starts,sequence,indicator,indent, lines[i], ends,linelength,endOfLineComment), starts, ends));
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
		                             bool endsLine, int linelength, string endOfLineComment) {
			var line = new StringBuilder();
			if (startsLine) {
				if (layout == ColumnsLayout.CobolReferenceFormat) {
					if (sequence != null) {
						if (sequence.Length != 6) {
							var msg = "Invalid sequence \""+sequence+"\" (expected length:6, actual:"+sequence.Length+")";
							throw new ArgumentException(msg);
						} else line.Append(sequence);
					} else line.Append("      ");
				}
				// if layout == ColumnsLayout.FreeTextFormat, drop sequence text
				line.Append(indicator);
				line.Append(indent);
			}
			line.Append(code);
			if (layout == ColumnsLayout.CobolReferenceFormat && endsLine && endOfLineComment != null) {
				linelength += line.Length;
				while(linelength++ < 72) line.Append(' ');
				line.Append(endOfLineComment);
			}
			return line.ToString();
		}
	}

}

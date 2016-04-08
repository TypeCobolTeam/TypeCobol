using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen {
	public class Generator {

		private ITextDocument Input;
		private System.IO.TextWriter Output;

		public Generator(System.IO.TextWriter destination, ITextDocument source) {
			Input = source;
			Output = destination;
		}

		/// <summary>Generates code</summary>
		/// <param name="tree">Root of a syntax tree</param>
		/// <param name="table">Table of symbols</param>
		/// <param name="columns">Columns layout</param>
		public void Generate(Node tree, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
			// we create a stack of text lines, so each tree node could choose to:
			// - write generated code without unstacking --> insert code
			// - unstack and write without modification  --> copy code
			// - unstack, modify and then write          --> alter code
			// - unstack and do nothing with it          --> delete code
			// each node could then pass the stack or not to its children
			var lines = new System.Collections.Generic.Stack<ITextLine>(Input.Lines);// lines are reversed...
			lines = new System.Collections.Generic.Stack<ITextLine>(lines);// ... so, put them back in order
			while(lines.Count > 0) Output.WriteLine(lines.Pop().Text);
			//tree.GenerateCode(stream, lines);
			// this stack thingie is far from perfect however, as a node far away in the tree traversal
			// (eg. in PROCEDURE DIVISION) could want to insert code between some lines that are already
			// written/unstacked (eg. in DATA DIVISION) ...
			// => we should rather do all the text modifications, and only then write stuff
		}
	}
}

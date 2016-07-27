using System;
using System.IO;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Generator
{
	// --> see http://stackoverflow.com/questions/20541073/translation-of-pl-sql-code-to-java-using-antlr-4-and-stringtemplate-4

	/// <summary>
	/// Code transformation tool which generates Cobol code from a TypeCobol source program
	/// </summary>
	public class TypeCobolGenerator
	{
		/// <summary>Source text document from wich program is parsed</summary>
		public ITextDocument Source { get; private set; }
		/// <summary>Source format</summary>
		public DocumentFormat Format { get; private set; }

		/// <summary>Parsed program</summary>
		public ProgramClassDocument Input { get; private set; }

		/// <summary>Initializes a TypeCobol to Cobol transformation.</summary>
		/// <param name="input">Parsed program</param>
		/// <param name="source">Source text document from wich program is parsed</param>
		public TypeCobolGenerator(ITextDocument source, DocumentFormat format, ProgramClassDocument program) {
			Input = program;
			Source = source;
			Format = format;
		}

		/// <summary>Will code be able to be generated?</summary>
		public bool IsValid {
			get {
				return Input != null && Input.Program != null;
			}
		}

		/// <summary>Generates Cobol program corresponding to the source file.</summary>
		public void WriteCobol(TextWriter stream) {
			int line = 1, offset = 0;
			WriteCobol(stream, Input.Program.SyntaxTree.Root, ref line, ref offset);
			stream.Close();
		}

		private void WriteCobol(TextWriter stream, Nodes.Node node, ref int line, ref int offset) {
			var ce = node.CodeElement as TypeCobol.Generator.CodeGenerator;
			if (ce != null) ce.WriteCode(stream, node.SymbolTable, ref line, ref offset, Format);
			foreach(var child in node.Children) {
				WriteCobol(stream, child, ref line, ref offset);
			}
		}
	}
}

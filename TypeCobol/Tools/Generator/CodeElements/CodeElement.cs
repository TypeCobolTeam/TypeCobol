using System.IO;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Generator;

namespace TypeCobol.Compiler.CodeElements {

	public abstract partial class CodeElement: CodeGenerator {

		/// <summary><see cref="TypeCobol.Generator.CodeGenerator"/></summary>
		public virtual void WriteCode(TextWriter stream, SymbolTable scope, ref int line, ref int offset, DocumentFormat format) {
			foreach(var token in ConsumedTokens) {
				WriteCode(stream, token, ref line, ref offset);
			}
		}

		internal static void WriteCode(TextWriter stream, Scanner.Token token, ref int line, ref int offset) {
			while(line < token.Line) {
				Codegen.WriteEmptyLine(stream, ref line, ref offset);
			}
			for(int c = offset; c < token.StartIndex; c++) {
				stream.Write(' ');
			}
			offset = token.StartIndex;
			stream.Write(token.Text);
			offset += token.Length;
		}
	}

}

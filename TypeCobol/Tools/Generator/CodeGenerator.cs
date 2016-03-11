namespace TypeCobol.Generator {

	/// <summary>An object able to be written as a programming language code.</summary>
	public interface CodeGenerator {
		/// <summary>Generates code corresponding to this object. Code is written on a <paramref name="stream"/> parameter.</summary>
		/// <param name="scope">Current scope (including declared variables and custom types)</param>
		/// <param name="line">in: last line written before this object / out: last line written by this object</param>
		/// <param name="offset">in: last column of <paramref name="line"/> written before this object / out: last column of <paramref name="line"/> written by this object</param>
		void WriteCode(System.IO.TextWriter stream, Compiler.CodeModel.SymbolTable scope, ref int line,ref int offset);
	}

	/// <summary>Code generation utilities static class.</summary>
	public static class Codegen {
		/// <summary>Generates an empty line on the <paramref name="stream"/> parameter.</summary>
		/// <param name="line">out: line++</param>
		/// <param name="offset">out: 0</param>
		public static void WriteEmptyLine(System.IO.TextWriter stream, ref int line, ref int offset) {
			stream.WriteLineAsync();
			line++;
			offset = 0;
		}
		public static void Write(System.IO.TextWriter stream, string s, ref int line, ref int offset) {
			stream.WriteAsync(s);
			offset += s.Length;
		}
		public static void WriteLine(System.IO.TextWriter stream, string s, ref int line, ref int offset) {
			Write(stream, s, ref line, ref offset);
			WriteEmptyLine(stream, ref line, ref offset);
		}
	}

}

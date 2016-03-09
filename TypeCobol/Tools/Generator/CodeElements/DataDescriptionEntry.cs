using System.IO;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Generator;

namespace TypeCobol.Compiler.CodeElements {

	public partial class DataDescriptionEntry {
		/// <summary><see cref="TypeCobol.Generator.CodeGenerator"/></summary>
		public override void WriteCode(TextWriter stream, SymbolTable scope, ref int line, ref int offset) {
			Codegen.WriteEmptyLine(stream, ref line, ref offset);
			if (IsTypeDefinitionPart) {
				return; // TYPEDEFS and subordinates have no equivalent in COBOL
			}
			if (scope.IsCustomType(this.DataType)) {
				string code = '\n'+LevelNumber.ToString("00")+" "+Name+".";
				Codegen.Write(stream, code, ref line, ref offset);
				return;
			}
			base.WriteCode(stream, scope, ref line, ref offset);
		}
	}

}

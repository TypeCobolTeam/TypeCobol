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
//			if (scope.IsCustomType(this.DataType)) {
//			if (IsGenerated(scope)) {
				string code = "";
				for (int c=0; c<Generation; c++) code += " ";
				code += LevelNumber.ToString("00")+" "+Name;
				if (!IsGroup && !scope.IsCustomType(DataType)) code += " PIC "+Picture;
				code += ".";
				Codegen.Write(stream, code, ref line, ref offset);
				return;
//			}
			base.WriteCode(stream, scope, ref line, ref offset);
		}

		/// <summary>All DataDescriptionEntries have one of their TopLevel items be of a custom type.</summary>
		/// <param name="scope">Custom types for this scope</param>
		/// <returns>True if the DataDescriptionEntry is generated (ie. not standard COBOL)</returns>
		private bool IsGenerated(SymbolTable scope) {
			var current = this.TopLevel;
			while(current != null) {
				if (scope.IsCustomType(current.DataType))
					return true;
				current = current.TopLevel;
			}
			return false;
		}
	}

}

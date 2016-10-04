﻿using System.IO;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Generator;

namespace TypeCobol.Compiler.CodeElements {

/*
	//TODO#249 DataDescriptionEntry.TopLevel no longer exists

	public partial class DataDescriptionEntry {
		/// <summary><see cref="TypeCobol.Generator.CodeGenerator"/></summary>
		public override void WriteCode(TextWriter stream, SymbolTable scope, ref int line, ref int offset, DocumentFormat format) {
			// TYPEDEFS and subordinates have no equivalent in COBOL
			// and thus must not be re-written
			//if (IsTypeDefinitionPart) return;

			int TrueParentGeneration;
			int HowFarRemoved;
			bool generated = IsGenerated(scope, out TrueParentGeneration, out HowFarRemoved);
			if (generated) {
				// fix indentation for custom or generated data declarations
				if (HowFarRemoved == 0) Codegen.WriteEmptyLine(stream, ref line, ref offset);
				string indent = "";
				if (format.ColumnsLayout == TypeCobol.Compiler.Text.ColumnsLayout.CobolReferenceFormat) {
					indent = "        "; // column:8 for level 01
					if (TopLevel != null) {
						indent += "    "; // column:12 for 1st generation subordinates
						var parent = TopLevel.TopLevel;
						while(parent != null) {
							indent += "  ";// column+2 for each subsequent generation
							parent = parent.TopLevel;
						}
					}
				} else {
					string indentUnit = "  ";
					for (int c=0; c < TrueParentGeneration+HowFarRemoved; c++) indent += indentUnit;
				}
				Codegen.Write(stream, indent, ref line, ref offset);
			}

			if (generated) {
				// write the proper level number according to data hierarchy
				Codegen.Write(stream, LevelNumber.ToString("00")+" ", ref line, ref offset);
				// filter "TYPE UserDefinedWord" from re-written code
				bool filter = false;
				for(int i=1; i<ConsumedTokens.Count; i++) {
					var token = ConsumedTokens[i];
					if (token.SourceText.Equals("TYPE")) filter = true;
					if (!filter) WriteCode(stream, token, ref line, ref offset);
					if (token.SourceText.Equals(DataType.Name)) filter = false;
				}
			} else {
				// write pure COBOL data declaration with no modification
				base.WriteCode(stream, scope, ref line, ref offset, format);
			}

			if (generated) {
				Codegen.WriteEmptyLine(stream, ref line, ref offset, false);
			}
		}



		/// <summary>
		/// All DataDescriptionEntries that either are of a custom type (TYPE declaration),
		/// or have one of their TopLevel items be of a custom type, must not be rewritten "as is".
		/// </summary>
		/// <param name="scope">Custom types for this scope</param>
		/// <param name="TrueParentGeneration">
		/// Generation of the first not generated data item found among this data top-level items.
		/// </param>
		/// <param name="HowFarRemoved">
		/// How many "generations" between this data declaration and its first "not generated"
		/// top-level item. If this data is a TYPE user, HowFarRemoved will be 0
		/// </param>
		/// <returns>True if this data uses TYPE or has a TYPE user among its top-level data.</returns>
		private bool IsGenerated(SymbolTable scope, out int TrueParentGeneration, out int HowFarRemoved) {
			TrueParentGeneration = Generation;
			HowFarRemoved = 0;
			var current = this;
			while(current != null) {
				if (scope.IsCustomType(current.DataType)) {
					TrueParentGeneration -= HowFarRemoved;
					return true;
				}
				HowFarRemoved++;
				current = current.TopLevel;
			}
			HowFarRemoved = 0;
			return false;
		}
	}
*/
}

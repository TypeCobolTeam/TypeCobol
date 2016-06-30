using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics {


	class ReadOnlyPropertiesChecker : NodeListener
	{

		private static string[] READONLY_DATATYPES = { "DATE", };

		public IList<Type> GetCodeElements() {
			return new List<Type> { typeof(TypeCobol.Compiler.CodeModel.SymbolWriter), };
		}
		public void OnNode(Node node, ParserRuleContext c, Program program) {
			var element = node.CodeElement as TypeCobol.Compiler.CodeModel.SymbolWriter;
			var table = program.SymbolTable;
			foreach (var pair in element.Symbols) {
				if (pair.Item2 == null) continue; // no receiving item
				var lr = table.Get(pair.Item2);
				if (lr.Count != 1) continue; // ambiguity or not referenced; not my job
				var receiving = lr[0];
				checkReadOnly(node.CodeElement, receiving);
			}
		}

		private static void checkReadOnly(CodeElement e, DataDescriptionEntry receiving) {
			if (receiving.TopLevel == null) return;
			if (receiving.TopLevel.DataType == null) return;
			foreach (var type in READONLY_DATATYPES) {
				if (type.Equals(receiving.TopLevel.DataType.Name.ToUpper())) {
					DiagnosticUtils.AddError(e, type + " properties are read-only");
				}
			}
		}
	}



	class FunctionChecker: NodeListener {
		public IList<Type> GetCodeElements() {
			return new List<Type> { typeof(TypeCobol.Compiler.CodeModel.IdentifierUser), };
		}

		public void OnNode(Node node, ParserRuleContext context, Program program) {
			var element = node.CodeElement as TypeCobol.Compiler.CodeModel.IdentifierUser;
			foreach (var identifier in element.Identifiers) {
				CheckIdentifier(node.CodeElement, program.SymbolTable, identifier);
			}
		}

		private static void CheckIdentifier(CodeElement e, SymbolTable table, Identifier identifier) {
			var fun = identifier as FunctionReference;
			if (fun == null) return;// we only check functions
			var def = table.GetFunction(fun.Name);
			if (def == null) return;// ambiguity is not our job
			if (fun.Parameters.Count > def.Parameters.Count) {
				var message = String.Format("Function {0} only takes {1} parameters", def.Name, def.Parameters.Count);
				DiagnosticUtils.AddError(e, message);
			}
			for (int c = 0; c < def.Parameters.Count; c++) {
				var expected = def.Parameters[c];
				if (c < fun.Parameters.Count) {
					var actual = fun.Parameters[c].Value;
					if (actual is Identifier) {
						var found = table.Get(((Identifier)actual).Name);
						if (found.Count != 1) continue;// ambiguity is not our job
						var type = found[0].DataType;
						// type check. please note:
						// 1- if only one of [actual|expected] types is null, overriden DataType.!= operator will detect of it
						// 2- if both are null, wee WANT it to break: in TypeCobol EVERYTHING should be typed,
						//    and things we cannot know their type as typed as DataType.Unknown (which is a non-null valid type).
						if (type == null || type != expected.Type) {
							var message = String.Format("Function {0} expected parameter {1} of type {2} (actual: {3})", def.Name, c+1, expected.Type, type);
							DiagnosticUtils.AddError(e, message);
						}
						var length = found[0].MemoryArea.Length;
						if (length > expected.Length) {
							var message = String.Format("Function {0} expected parameter {1} of max length {2} (actual: {3})", def.Name, c+1, expected.Length, length);
							DiagnosticUtils.AddError(e, message);
						}
					}
				} else {
					var message = String.Format("Function {0} is missing parameter {1} of type {2}", def.Name, c+1, expected.Type);
					DiagnosticUtils.AddError(e, message);
				}
			}
		}
	}


}
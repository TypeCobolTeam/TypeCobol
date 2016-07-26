using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.CodeElements.Functions;

namespace TypeCobol.Compiler.Diagnostics {


	class ReadOnlyPropertiesChecker: NodeListener {

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
//TODO#249				checkReadOnly(node.CodeElement, receiving);
			}
		}
/*
		private static void checkReadOnly(CodeElement e, DataDescriptionEntry receiving) {
			if (receiving.TopLevel == null) return;
		    foreach (var type in READONLY_DATATYPES) {
				if (type.Equals(receiving.TopLevel.DataType.Name.ToUpper())) {
					DiagnosticUtils.AddError(e, type + " properties are read-only");
				}
			}
		}
*/
	}


/*TODO#249
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
			if (fun.Parameters.Count > def.InputParameters.Count) {
				var message = String.Format("Function {0} only takes {1} parameters", def.Name, def.InputParameters.Count);
				DiagnosticUtils.AddError(e, message);
			}
			for (int c = 0; c < def.InputParameters.Count; c++) {
				var expected = def.InputParameters[c];
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
*/



	class FunctionDeclarationChecker: NodeListener {

		public IList<Type> GetCodeElements() {
			return new List<Type> { typeof(FunctionDeclarationHeader), };
		}
		public void OnNode(Node node, ParserRuleContext context, Program program) {
			var header = node.CodeElement as FunctionDeclarationHeader;
			var visibility = header.Visibility;
			IList<InputParameter> inputs = new List<InputParameter>();
			IList<ReceivingStorageArea> outputs = new List<ReceivingStorageArea>();
			Node profile = null;
			var profiles = node.GetChildren(typeof(FunctionDeclarationProfile));
			if (profiles.Count < 1) // no PROCEDURE DIVISION internal to function
				DiagnosticUtils.AddError(header, "Function \""+header.Name+"\" has no parameters and does nothing.");
			else if (profiles.Count > 1)
				foreach(var p in profiles)
					DiagnosticUtils.AddError(p.CodeElement, "Function \""+header.Name+"\" can have only one parameters profile.");
			else profile = profiles[0];
			if (profile != null) {
				var p = ((FunctionDeclarationProfile)profile.CodeElement);
				inputs  = p.InputParameters;
				outputs = p.OutputParameters;
			}
			var parametersdeclared = new List<Parameter>();
			var linkage = node.Get("linkage");
			if (linkage == null) {
				if (inputs.Count > 0 || outputs.Count > 0)
					DiagnosticUtils.AddError(header, "Missing LINKAGE SECTION for parameters declaration.");
			} else {
			    var data = linkage.GetChildren(typeof(DataDescriptionEntry));
				foreach(var n in data) {
					var d = (DataDescriptionEntry)n.CodeElement;
					bool custom = false;//TODO
//TODO#249					parametersdeclared.Add(new Parameter(d.DataName.Name, custom, d.DataType, d.MemoryArea.Length));
				}
			}
			var inparameters = new List<Parameter>();
			foreach(var p in inputs) {
//TODO#249				string pname = p.DataName.Name;
//				Parameter param = GetParameter(parametersdeclared, pname);
//				if (param != null) inparameters.Add(param);
//				else DiagnosticUtils.AddError(profile.CodeElement, pname+" undeclared in LINKAGE SECTION.");
			}
			var outparameters = new List<Parameter>();
			foreach(var p in outputs) {
//TODO#249				string pname = p.Name;
//				Parameter param = GetParameter(parametersdeclared, pname);
//				if (param != null) outparameters.Add(param);
//				else DiagnosticUtils.AddError(profile.CodeElement, pname+" undeclared in LINKAGE SECTION.");
			}
			if (outparameters.Count < 1) outparameters.Add(new Parameter("return-code", false, DataType.Numeric));
			foreach(var pd in parametersdeclared) {
				var used = GetParameter(inparameters, pd.Name);
				if (used == null)
					used = GetParameter(outparameters, pd.Name);
				if (used == null) {
					var data = GetParameter(linkage, pd.Name);
					DiagnosticUtils.AddError(data, pd.Name+" is not a parameter.");
				}
			}
			var function = new Function(header.Name, inparameters, outparameters, visibility);
			node.SymbolTable.EnclosingScope.Register(function);
		}

		private Parameter GetParameter(IList<Parameter> parameters, string name) {
			foreach(var p in parameters)
				if (p.Name.Equals(name)) return p;
			return null;
		}
		private DataDescriptionEntry GetParameter(Node node, string name) {
			var data = node.CodeElement as DataDescriptionEntry;
//TODO#249			if (data != null && data.QualifiedName.Matches(name)) return data;
			foreach(var child in node.Children) {
				var found = GetParameter(child, name);
				if (found != null) return found;
			}
			return null;
		}
	}


}
using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.Diagnostics {


class ReadOnlyPropertiesChecker: NodeListener {

	private static string[] READONLY_DATATYPES = { "DATE", };

	public IList<Type> GetNodes() { return new List<Type> { typeof(VariableWriter), }; }
	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
		var element = node.CodeElement as VariableWriter;
		var table = program.SymbolTable;
		foreach (var pair in element.VariablesWritten) {
			if (pair.Key == null) continue; // no receiving item
			var lr = table.GetVariable(pair.Key);
			if (lr.Count != 1) continue; // ambiguity or not referenced; not my job
			var receiving = lr[0];
			checkReadOnly(node.CodeElement, receiving as Node);
		}
	}
	private void checkReadOnly(CodeElement ce, Node receiving) {
		var rtype = receiving.Parent as ITypedNode;
		if (rtype == null) return;
		foreach(var type in READONLY_DATATYPES) {
			if (type.Equals(rtype.DataType.Name.ToUpper()))
				DiagnosticUtils.AddError(ce, type+" properties are read-only");
		}
	}
}


class FunctionCallChecker: NodeListener {

	public IList<Type> GetNodes() {
		return new List<Type> { typeof(FunctionCaller), };
	}
	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
		var statement = node as FunctionCaller;

		foreach(var fun in statement.FunctionCalls) {
			var found = node.SymbolTable.GetFunction(new URI(fun.FunctionName));
			if (found.Count != 1) continue; // ambiguity is not our job
			var declaration = (FunctionDeclaration)found[0];
			Check(node.CodeElement, node.SymbolTable, fun, declaration);
		}
	}
	private void Check(CodeElement e, SymbolTable table, CodeElements.FunctionCall call, FunctionDeclaration definition) {
		var parameters = definition.Profile.Parameters;
        var callArgsCount = call.Arguments != null ? call.Arguments.Length : 0;
        if (callArgsCount > parameters.Count) {
			var m = System.String.Format("Function {0} only takes {1} parameters", definition.Name, parameters.Count);
			DiagnosticUtils.AddError(e, m);
		}
		for (int c = 0; c < parameters.Count; c++) {
			var expected = parameters[c];
			if (c < callArgsCount) {
				var actual = call.Arguments[c].StorageAreaOrValue;
				if (actual.IsLiteral) continue;
                var callArgName = actual.MainSymbolReference != null ? actual.MainSymbolReference.Name : null;
                var found = table.GetVariable(new URI(callArgName));
				if (found.Count < 1) DiagnosticUtils.AddError(e, "Parameter "+callArgName+" is not referenced");
				if (found.Count > 1) DiagnosticUtils.AddError(e, "Ambiguous reference to parameter "+callArgName);
				if (found.Count!= 1) continue;
				var type = found[0] as ITypedNode;
				// type check. please note:
				// 1- if only one of [actual|expected] types is null, overriden DataType.!= operator will detect it
				// 2- if both are null, we WANT it to break: in TypeCobol EVERYTHING should be typed,
				//    and things we cannot know their type as typed as DataType.Unknown (which is a non-null valid type).
				if (type == null || type.DataType != expected.DataType) {
					var m = System.String.Format("Function {0} expected parameter {1} of type {2} (actual: {3})", definition.Name, c+1, expected.DataType, type.DataType);
					DiagnosticUtils.AddError(e, m);
				}
				if (type != null && type.Length > expected.Length) {
					var m = System.String.Format("Function {0} expected parameter {1} of max length {2} (actual: {3})", definition.Name, c+1, expected.Length, type.Length);
					DiagnosticUtils.AddError(e, m);
				}
			} else {
				var m = System.String.Format("Function {0} is missing parameter {1} of type {2}", definition.Name, c+1, expected.DataType);
				DiagnosticUtils.AddError(e, m);
			}
		}
	}
}



class FunctionDeclarationChecker: NodeListener {

	public IList<Type> GetNodes() {
		return new List<Type> { typeof(FunctionDeclaration), };
	}
	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
		var header = node.CodeElement as FunctionDeclarationHeader;
		var filesection = node.Get<FileSection>("file");
		if (filesection != null) // TCRFUN_DECLARATION_NO_FILE_SECTION
			DiagnosticUtils.AddError(filesection.CodeElement, "Illegal FILE SECTION in function \""+header.Name+"\" declaration", context);

		CheckNoGlobalOrExternal(node.Get<DataDivision>("data-division"));

		CheckParameters(header.Profile, header, context);
		CheckNoLinkageItemIsAParameter(node.Get<LinkageSection>("linkage"), header.Profile);

		var functions = node.SymbolTable.GetFunction(new URI(header.Name), header.Profile);
		if (functions.Count > 1)
			DiagnosticUtils.AddError(header, "A function \""+new URI(header.Name).Head+"\" with the same profile already exists in namespace \""+new URI(header.Name).Tail+"\".", context);
//		foreach(var function in functions) {
//			if (!function.IsProcedure && !function.IsFunction)
//				DiagnosticUtils.AddError(header, "\""+header.Name.Head+"\" is neither procedure nor function.", context);
//		}
	}

	private void CheckNoGlobalOrExternal(DataDivision node) {
		if (node == null) return; // no DATA DIVISION
		foreach(var section in node.Children()) { // "storage" sections
			foreach(var child in section.Children) {
			        var data = child.CodeElement as DataDescriptionEntry;
			        if (data == null) continue;
			        if (data.IsGlobal) // TCRFUN_DECLARATION_NO_GLOBAL
			            DiagnosticUtils.AddError(data, "Illegal GLOBAL clause in function data item.");
			        if (data.IsExternal) // TCRFUN_DECLARATION_NO_EXTERNAL
			            DiagnosticUtils.AddError(data, "Illegal EXTERNAL clause in function data item.");
			}
		}
	}

	private void CheckParameters(ParametersProfile profile, CodeElement ce, ParserRuleContext context) {
		foreach(var parameter in profile.InputParameters)  CheckParameter(parameter, ce, context);
		foreach(var parameter in profile.InoutParameters)  CheckParameter(parameter, ce, context);
		foreach(var parameter in profile.OutputParameters) CheckParameter(parameter, ce, context);
		if (profile.ReturningParameter != null) CheckParameter(profile.ReturningParameter, ce, context);
	}
	private void CheckParameter(ParameterDescriptionEntry parameter, CodeElement ce, ParserRuleContext context) {
		// TCRFUN_LEVEL_88_PARAMETERS
		if (parameter.LevelNumber.Value != 1)
		DiagnosticUtils.AddError(ce, "Condition parameter \""+parameter.Name+"\" must be subordinate to another parameter.", context);
        if (parameter.DataConditions != null)
        {
            foreach (var condition in parameter.DataConditions)
            {
                if (condition.LevelNumber.Value != 88)
                    DiagnosticUtils.AddError(ce, "Condition parameter \"" + condition.Name + "\" must be level 88.");
            }
        }
	}
	/// <summary>TCRFUN_DECLARATION_NO_DUPLICATE_NAME</summary>
	/// <param name="node">LINKAGE SECTION node</param>
	/// <param name="profile">Parameters for original function</param>
	private void CheckNoLinkageItemIsAParameter(LinkageSection node, ParametersProfile profile) {
		if (node == null) return; // no LINKAGE SECTION
		var linkage = new List<DataDefinition>();
		AddEntries(linkage, node);
		foreach(var description in linkage) {
			var used = Validate(profile.ReturningParameter, description.Name);
			if (used != null) { AddErrorAlreadyParameter(description, description.QualifiedName); continue; }
			used = GetParameter(profile.InputParameters,  description.Name);
			if (used != null) { AddErrorAlreadyParameter(description, description.QualifiedName); continue; }
			used = GetParameter(profile.OutputParameters, description.Name);
			if (used != null) { AddErrorAlreadyParameter(description, description.QualifiedName); continue; }
			used = GetParameter(profile.InoutParameters,  description.Name);
			if (used != null) { AddErrorAlreadyParameter(description, description.QualifiedName); continue; }
		}
	}
	private void AddEntries(List<DataDefinition> linkage, LinkageSection node) {
		foreach(var definition in node.Children())
			AddEntries(linkage, definition);
	}
	private void AddEntries(List<DataDefinition> linkage, DataDefinition node) {
		linkage.Add(node);
		foreach(var child in node.Children())
			AddEntries(linkage, child);
	}
	private ParameterDescriptionEntry GetParameter(IList<ParameterDescriptionEntry> parameters, string name) {
		if (name == null) return null;
		foreach(var p in parameters)
			if (Validate(p, name) != null) return p;
		return null;
	}
	private ParameterDescriptionEntry Validate(ParameterDescriptionEntry parameter, string name) {
		if (parameter != null && parameter.Name.Equals(name)) return parameter;
		return null;
	}
	private void AddErrorAlreadyParameter(Node node, QualifiedName name) {
		DiagnosticUtils.AddError(node.CodeElement, name.Head+" is already a parameter.");
	}
}



/// <summary>Checks the TypeCobol custom functions rules:
/// * TCRFUN_NO_SECTION_OR_PARAGRAPH_IN_LIBRARY
/// * TCRFUN_LIBRARY_COPY
/// </summary>
class LibraryChecker: NodeListener {
	public IList<Type> GetNodes() {
		return new List<Type> { typeof(ProcedureDivision), };
	}
	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
		var pdiv = node.CodeElement as ProcedureDivisionHeader;
		bool isPublicLibrary = false;
		var elementsInError = new List<CodeElement>();
		var errorMessages = new List<string>();
		foreach(var child in node.Children) {
			var ce = child.CodeElement;
			if (child.CodeElement == null) {
				elementsInError.Add(node.CodeElement);
				errorMessages.Add("Illegal default section in library.");
			} else {
				var function = child.CodeElement as FunctionDeclarationHeader;
				if (function != null) {
					isPublicLibrary = isPublicLibrary || function.Visibility == AccessModifier.Public;
				} else {
					elementsInError.Add(child.CodeElement);
					errorMessages.Add("Illegal non-function item in library");
				}
			}
		}
		var pgm = (Nodes.Program)node.Root.GetChildren<ProgramIdentification>()[0];
		var copies = pgm.GetChildren<LibraryCopyCodeElement>();
		var copy = copies.Count > 0? ((LibraryCopy)copies[0]) : null;
		if (isPublicLibrary) {
			if (copy == null || copy.CodeElement().Name == null)
				DiagnosticUtils.AddError(pgm.CodeElement, "Missing library copy in IDENTIFICATION DIVISION.", context);

			if (pdiv.UsingParameters != null && pdiv.UsingParameters.Count > 0)
				DiagnosticUtils.AddError(pdiv, "Illegal "+pdiv.UsingParameters.Count+" USING in library PROCEDURE DIVISION.", context);

			for(int c = 0; c < errorMessages.Count; c++)
				DiagnosticUtils.AddError(elementsInError[c], errorMessages[c], context);
		}
	}
}


}

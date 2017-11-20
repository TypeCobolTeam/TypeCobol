using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using System.Diagnostics;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;
using System.Linq;
using Analytics;
using TypeCobol.Compiler.Scanner;
using System.Text.RegularExpressions;

namespace TypeCobol.Compiler.Diagnostics {


class ReadOnlyPropertiesChecker {

	private static string[] READONLY_DATATYPES = { "DATE", };

	public static void OnNode([NotNull] Node node) {
	    VariableWriter variableWriter = node as VariableWriter;
	    if (variableWriter == null) {
	        return; //not our job
	    }
        var element = node.CodeElement as VariableWriter;
		var table = node.SymbolTable;
		foreach (var pair in element.VariablesWritten) {
			if (pair.Key == null) continue; // no receiving item
			var lr = table.GetVariables(pair.Key);
			if (lr.Count != 1) continue; // ambiguity or not referenced; not my job
			var receiving = lr[0];
			checkReadOnly(node, receiving);
		}
	}
	private static void checkReadOnly(Node node, [NotNull] Node receiving) {
		var rtype = receiving.Parent as ITypedNode;
		if (rtype == null) return;
		foreach(var type in READONLY_DATATYPES) {
			if (type.Equals(rtype.DataType.Name.ToUpper()))
				DiagnosticUtils.AddError(node, type+" properties are read-only");
		}
	}
}


    class FunctionCallChecker
    {

        public static void OnNode(Node node)
        {
            var functionCaller = node as FunctionCaller;
            if (functionCaller == null || functionCaller.FunctionCall == null || !functionCaller.FunctionCall.NeedDeclaration)
                return;

            AnalyticsWrapper.Telemetry.TrackEvent("[Function-Call] " + functionCaller.FunctionCall.FunctionName);

            if (functionCaller.FunctionDeclaration == null)
            {
                //Get Funtion by name and profile (matches on precise parameters)
                var parameterList = functionCaller.FunctionCall.AsProfile(node.SymbolTable);
                var functionDeclarations = node.SymbolTable.GetFunction(new URI(functionCaller.FunctionCall.FunctionName),
                    parameterList, functionCaller.FunctionCall.Namespace);

                string message;
                //There is one CallSite per function call
                //This is a call to a TypeCobol function or procedure with arguments
                if (node.CodeElement.CallSites.Count == 1 && node.CodeElement.CallSites[0].CallTarget.IsOrCanBeOnlyOfTypes(SymbolType.TCFunctionName))
                {
                    if (functionDeclarations.Count == 1)
                    {
                        functionCaller.FunctionDeclaration = functionDeclarations.First();
                        Check(node, functionCaller.FunctionCall, functionCaller.FunctionDeclaration);
                        return; //Everything seems to be ok, lets continue on the next one
                    }
                    //Another checker should check if function declaration is not duplicated
                    if (functionDeclarations.Count > 0)
                    {
                        message = string.Format("Same function '{0}' {1} declared '{2}' times", functionCaller.FunctionCall.FunctionName, parameterList.GetSignature(), functionDeclarations.Count);
                        DiagnosticUtils.AddError(node, message, MessageCode.ImplementationError);
                        return; //Do not continue the function/procedure is defined multiple times
                    }

                    var otherDeclarations =
                        node.SymbolTable.GetFunction(((ProcedureCall)functionCaller.FunctionCall).ProcedureName.URI, null, functionCaller.FunctionCall.Namespace);

                    if (functionDeclarations.Count == 0 && otherDeclarations.Count == 0)
                    {
                        message = string.Format("Function not found '{0}' {1}", functionCaller.FunctionCall.FunctionName, parameterList.GetSignature());
                        DiagnosticUtils.AddError(node, message);
                        return; //Do not continue the function/procedure does not exists
                    }
                    if (otherDeclarations.Count > 1)
                    {
                        message = string.Format("No suitable function signature found for '{0}' {1}", functionCaller.FunctionCall.FunctionName, parameterList.GetSignature());
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    functionDeclarations = otherDeclarations;
                }
                else
                {
                    //call to a TypeCobol function/procedure without arguments or to a Variable

                    var potentialVariables = node.SymbolTable.GetVariables(new URI(functionCaller.FunctionCall.FunctionName));

                    if (functionDeclarations.Count == 1 && potentialVariables.Count == 0)
                    {
                        functionCaller.FunctionDeclaration = functionDeclarations.First();
                        return; //Everything seems to be ok, lets continue on the next one
                    }

                    functionDeclarations =
                           node.SymbolTable.GetFunction(new URI(functionCaller.FunctionCall.FunctionName), null, functionCaller.FunctionCall.Namespace);

                    if (potentialVariables.Count > 1)
                    {
                        //If there is more than one variable with the same name, it's ambiguous
                        message = string.Format("Call to '{0}'(no arguments) is ambigous. '{0}' is defined {1} times", functionCaller.FunctionCall.FunctionName, potentialVariables.Count + functionDeclarations.Count);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count > 1 && potentialVariables.Count == 0)
                    {
                        message = string.Format("No suitable function signature found for '{0}(no arguments)'", functionCaller.FunctionCall.FunctionName);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count >= 1 && potentialVariables.Count == 1)
                    {
                        message = string.Format("Warning: Risk of confusion in call of '{0}'", functionCaller.FunctionCall.FunctionName);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count == 0 && potentialVariables.Count == 0)
                    {
                        message = string.Format("No function or variable found for '{0}'(no arguments)", functionCaller.FunctionCall.FunctionName);
                        DiagnosticUtils.AddError(node, message);
                        return; //Do not continue the function/procedure does not exists
                    }

                    if (potentialVariables.Count == 1)
                        return; //Stop here, it's a standard Cobol call
                }


                functionCaller.FunctionDeclaration = functionDeclarations[0];
                //If function is not ambigous and exists, lets check the parameters
                Check(node, functionCaller.FunctionCall, functionCaller.FunctionDeclaration);
            }
        }





        private static void Check(Node node, [NotNull] FunctionCall call,
            [NotNull] FunctionDeclaration definition)
        {
            var table = node.SymbolTable;
            var parameters = definition.Profile.Parameters;
            var callArgsCount = call.Arguments != null ? call.Arguments.Length : 0;
            if (callArgsCount > parameters.Count)
            {
                var m = string.Format("Function '{0}' only takes {1} parameter(s)", call.FunctionName, parameters.Count);
                DiagnosticUtils.AddError(node, m);
            }
            for (int c = 0; c < parameters.Count; c++)
            {
                var expected = parameters[c];
                if (c < callArgsCount)
                {
                    //Omitted
                    if (call.Arguments[c].IsOmitted)
                    {
                        if (expected.IsOmittable) {
                            continue;
                        } else {
                            DiagnosticUtils.AddError(node, "Omitted not allowed for this parameter");
                            return;
                        }
                    }
                    
                    
                    var actual = call.Arguments[c].StorageAreaOrValue;
                    if (actual.IsLiteral) continue; //TODO


                    var actualSpecialRegister = actual.StorageArea as StorageAreaPropertySpecialRegister;
                    if (actualSpecialRegister != null) {
                        var tokenType = actualSpecialRegister.SpecialRegisterName.TokenType;
                        if (tokenType == TokenType.LENGTH) {
                            //parameter must be a Numeric of lengt
                            //TODO
                            //return an error for now
                            DiagnosticUtils.AddError(node, "LENGTH OF not allowed yet with procedure");
                            return;
                        } else if (tokenType == TokenType.ADDRESS && expected.Usage == DataUsage.Pointer) {
                            //It's ok
                            return;
                        } else if (tokenType == TokenType.LINAGE_COUNTER) {
                            //Do not know what to do : RFC
                            DiagnosticUtils.AddError(node, "LENGTH OF not allowed yet with procedure");
                            return;
                        }
                    }

                    var callArgName = actual.MainSymbolReference != null ? actual.MainSymbolReference.Name : null;
                    var found = table.GetVariables(actual);
                    if (found.Count != 1) continue; //Diagnostics have already been generated by Cobol85Checker
                    var actualDataDefinition = found[0];

                    //TODO use SubscriptExpression and ReferenceModifier of the StorageArea to correct the type
                    //Ex: MyVar1(1:10) has a length of 10 and is of type Alphanumeric
                    //Ex: MyArray(1) only target one element of the array, so we need to get the type of this element.


                    //Cobol 85 Type will be checked with their picture
                    if (actualDataDefinition.DataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85 || expected.DataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85)
                    {
                        if (actualDataDefinition.DataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85 ||
                            expected.DataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85) {
                            var m = string.Format(
                                    "Function '{0}' expected parameter '{1}' of type {2} and received '{3}' of type {4} ",
                                    call.FunctionName, expected.Name, expected.DataType,
                                    callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.DataType);
                            DiagnosticUtils.AddError(node, m);

                            
                        } else if (actualDataDefinition.DataType != expected.DataType) {
                            DataDefinition callerType = GetSymbolType(actualDataDefinition);
                            DataDefinition calleeType = GetSymbolType(expected);
                            if (callerType == null || calleeType == null) {
                                //Ignore, it's an unknown DataType. It's already checked
                            } else if (!Equals(callerType.QualifiedName, calleeType.QualifiedName)) {
                                var m = string.Format(
                                        "Function '{0}' expected parameter '{1}' of type {2} and received '{3}' of type {4} ",
                                        call.FunctionName, calleeType.Name, calleeType.DataType,
                                        callArgName ?? string.Format("position {0}", c + 1), callerType.DataType);
                                DiagnosticUtils.AddError(node, m);
                            }
                        }
                    }

                    if(actualDataDefinition.Picture != null && expected.Picture != null && actualDataDefinition.Picture.NormalizedValue != expected.Picture.NormalizedValue)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' with picture {2} and received '{3}' with picture {4}",
                                call.FunctionName, expected.Name, expected.Picture.Value,
                                callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.Picture.Value);
                        DiagnosticUtils.AddError(node, m);
                    }
              

//                    if (dataDefinitionOfActual.Length != expectedParameter.Length)
//                    {
//                        var m =
//                            string.Format(
//                                "Function '{0}' expected parameter '{1}' of length {2} and received '{3}' of length {4}",
//                                call.FunctionName, expectedParameter.Name, expectedParameter.Length,
//                                callArgName ?? string.Format("position {0}", c + 1), dataDefinitionOfActual.Length);
//                        DiagnosticUtils.AddError(e, m);
//                    }

                    if (actualDataDefinition.Usage != expected.Usage)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' of usage {2} and received '{3}' of usage {4}",
                                call.FunctionName, expected.Name, expected.Usage,
                                callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.Usage);
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.IsJustified != expected.IsJustified)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                call.FunctionName, expected.Name, expected.IsJustified ? "justified" : "non-justified",
                                callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.IsJustified ? "justified" : "non-justified");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.IsGroupUsageNational != expected.IsGroupUsageNational)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                               call.FunctionName, expected.Name, expected.IsGroupUsageNational ? "national group-usage" : "non national group-usage",
                               callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.IsGroupUsageNational ? "national group-usage" : "non national group-usage");
                        DiagnosticUtils.AddError(node, m);
                    }
                    if (actualDataDefinition.IsTableOccurence != expected.IsTableOccurence) {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' to {2} an array and received '{3}' which {4} an array",
                               call.FunctionName, expected.Name, expected.IsTableOccurence ? "be" : "be NOT", actualDataDefinition.Name,
                                actualDataDefinition.IsTableOccurence ? "is" : "is NOT ");
                        DiagnosticUtils.AddError(node, m);
                    } else if (actualDataDefinition.IsTableOccurence && expected.IsTableOccurence) {
                        if (actualDataDefinition.MinOccurencesCount != expected.MinOccurencesCount) {
                            var m =
                                string.Format(
                                    "Function '{0}' expected parameter '{1}' to have at least {2} occurences and received '{3}' with a minimum of {4} occurences",
                                    call.FunctionName, expected.Name, expected.MinOccurencesCount,
                                    callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.MinOccurencesCount);
                            DiagnosticUtils.AddError(node, m);
                        }

                        if (actualDataDefinition.MaxOccurencesCount != expected.MaxOccurencesCount) {
                            var m =
                                string.Format(
                                    "Function '{0}' expected parameter '{1}' to have at most {2} occurences and received '{3}' with a maximum of {4} occurences",
                                    call.FunctionName, expected.Name, expected.MaxOccurencesCount,
                                    callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.MaxOccurencesCount);
                            DiagnosticUtils.AddError(node, m);
                        }
                    }

                    if (actualDataDefinition.OccursDependingOn != expected.OccursDependingOn)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' occurs depending on ({2}) occurences and received '{3}' occurs depending on ({4})",
                               call.FunctionName, expected.Name, expected.OccursDependingOn,
                               callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.OccursDependingOn);
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.HasUnboundedNumberOfOccurences != expected.HasUnboundedNumberOfOccurences)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                               call.FunctionName, expected.Name, expected.HasUnboundedNumberOfOccurences ? "has unbounded number of occurences" : "hasn't unbounded number of occurences",
                               callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.HasUnboundedNumberOfOccurences ? "has unbounded number of occurences" : "hasn't unbounded number of occurences");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.SignIsSeparate != expected.SignIsSeparate)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                               call.FunctionName, expected.Name, expected.HasUnboundedNumberOfOccurences ? "has unbounded number of occurences" : "hasn't unbounded number of occurences",
                               callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.HasUnboundedNumberOfOccurences ? "has unbounded number of occurences" : "hasn't unbounded number of occurences");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.SignPosition != expected.SignPosition)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' with sign position {2} and received '{3}' with sign position {4}",
                               call.FunctionName, expected.Name, expected.SignPosition == null ? "empty" : expected.SignPosition.ToString(),
                               callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.SignPosition == null ? "empty" : actualDataDefinition.SignPosition.ToString());
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.IsSynchronized != expected.IsSynchronized)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                               call.FunctionName, expected.Name, expected.IsSynchronized ? "synchonized" : "not synchronized",
                               callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.IsSynchronized ? "synchonized" : "not synchronized");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.ObjectReferenceClass != expected.ObjectReferenceClass)
                    {
                        var m =
                          string.Format(
                              "Function '{0}' expected parameter '{1}' and received '{2}' with wrong object reference.",
                              call.FunctionName, expected.Name, callArgName ?? string.Format("position {0}", c + 1));
                        DiagnosticUtils.AddError(node, m);
                    }
                }
                else
                {
                    var m = string.Format("Function '{0}' is missing parameter '{1}' of type {2} and length {3}",
                        call.FunctionName, expected.Name, expected.DataType, expected.Length);
                    DiagnosticUtils.AddError(node, m);
                }
            }
        }

        private static TypeDefinition GetSymbolType(DataDefinition node)
        {
            var found = node.SymbolTable.GetType(node.DataType);

            if (found != null)
                return found.FirstOrDefault();
            else
                return null;
        }
    }


    class FunctionDeclarationTypeChecker: CodeElementListener {

	public void OnCodeElement(CodeElement ce, ParserRuleContext context) {
		var function = ce as FunctionDeclarationHeader;
	    if (function == null) {
                return;//not my job
	    }
		if (function.ActualType == FunctionType.Undefined) {
			DiagnosticUtils.AddError(ce, "Incompatible parameter clauses for "+ToString(function.UserDefinedType)+" \""+function.Name+"\"", context);
		} else
		if (  (function.ActualType == FunctionType.Function && function.UserDefinedType == FunctionType.Procedure)
			||(function.ActualType == FunctionType.Procedure && function.UserDefinedType == FunctionType.Function) ) {
			var message = "Symbol \""+function.Name+"\" is defined as "+ToString(function.UserDefinedType)
						+", but parameter clauses describe a "+ToString(function.ActualType);
			DiagnosticUtils.AddError(ce, message, context);
		}
	}
	private string ToString(FunctionType type) {
		if (type == FunctionType.Undefined) return "symbol";
		if (type == FunctionType.Function) return "function";
		if (type == FunctionType.Procedure) return "procedure";
		return "function or procedure";
	}
}

class FunctionDeclarationChecker: NodeListener {

	public IList<Type> GetNodes() {
		return new List<Type> { typeof(FunctionDeclaration), };
	}
	public void OnNode([NotNull] Node node, ParserRuleContext context, CodeModel.Program program) {
            FunctionDeclaration functionDeclaration = node as FunctionDeclaration;
            if (functionDeclaration == null) return; //not my job
            var header = node.CodeElement as FunctionDeclarationHeader;
	    if (header == null) return; //not my job
		var filesection = node.Get<FileSection>("file");
		if (filesection != null) // TCRFUN_DECLARATION_NO_FILE_SECTION
			DiagnosticUtils.AddError(filesection, "Illegal FILE SECTION in function \""+header.Name+"\" declaration", context);

		CheckNoGlobalOrExternal(node.Get<DataDivision>("data-division"));

		CheckParameters(header.Profile, context, node);
		CheckNoLinkageItemIsAParameter(node.Get<LinkageSection>("linkage"), header.Profile);

		CheckNoPerform(node.SymbolTable.EnclosingScope, node);

	    var headerNameURI = new URI(header.Name);
	    var functions = node.SymbolTable.GetFunction(headerNameURI, functionDeclaration.Profile);
		if (functions.Count > 1)
			DiagnosticUtils.AddError(node, "A function \""+headerNameURI.Head+"\" with the same profile already exists in namespace \""+headerNameURI.Tail+"\".", context);
//		foreach(var function in functions) {
//			if (!function.IsProcedure && !function.IsFunction)
//				DiagnosticUtils.AddError(node, "\""+header.Name.Head+"\" is neither procedure nor function.", context);
//		}
	}

	private void CheckNoGlobalOrExternal(DataDivision node) {
		if (node == null) return; // no DATA DIVISION
		foreach(var section in node.Children()) { // "storage" sections
			foreach(var child in section.Children) {
			        var data = child.CodeElement as DataDescriptionEntry;
			        if (data == null) continue;
			        if (data.IsGlobal) // TCRFUN_DECLARATION_NO_GLOBAL
			            DiagnosticUtils.AddError(child, "Illegal GLOBAL clause in function data item.");
			}
		}
	}

    private void CheckParameters([NotNull] ParametersProfile profile, ParserRuleContext context, Node node)
    {
        var parameters = profile.Parameters;
        foreach (var parameter in profile.InputParameters) CheckParameter(parameter, context, node);
        foreach (var parameter in profile.InoutParameters) CheckParameter(parameter, context, node);
        foreach (var parameter in profile.OutputParameters) CheckParameter(parameter, context, node);
        if (profile.ReturningParameter != null)
        {
            CheckParameter(profile.ReturningParameter, context, node);
            parameters.Add(profile.ReturningParameter);
        }

        foreach (var duplicatedParameter in parameters.GroupBy(p => p.Name).Where(g => g.Skip(1).Any()).SelectMany(g => g))//Group on parameter.Name //where group contains more than one item //reexpand to get all duplicated parameters 
        {
            DiagnosticUtils.AddError(node,
                string.Format("Parameter with name '{0}' declared multiple times", duplicatedParameter.Name));
        }


    }

    private void CheckParameter([NotNull] ParameterDescriptionEntry parameter,  ParserRuleContext context, Node node)
        {
            // TCRFUN_LEVEL_88_PARAMETERS
            if (parameter.LevelNumber.Value != 1)
            {
                DiagnosticUtils.AddError(node, "Condition parameter \"" + parameter.Name + "\" must be subordinate to another parameter.", context);
            }
            if (parameter.DataConditions != null)
            {
                foreach (var condition in parameter.DataConditions)
                {
                    if (condition.LevelNumber.Value != 88)
                        DiagnosticUtils.AddError(node, "Condition parameter \"" + condition.Name + "\" must be level 88.");
                }
            }

            if (parameter.Picture != null)
            {
                Cobol85CompleteASTChecker.CheckPicture(node, parameter);
            }

            var type = parameter.DataType;
            TypeDefinitionHelper.Check(node, type); //Check if the type exists and is not ambiguous

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
	private void AddEntries([NotNull] List<DataDefinition> linkage, DataDefinition node) {
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
	private void AddErrorAlreadyParameter([NotNull] Node node, [NotNull] QualifiedName name) {
		DiagnosticUtils.AddError(node, name.Head+" is already a parameter.");
	}

	private void CheckNoPerform(SymbolTable table, [NotNull] Node node) {
		if (node is PerformProcedure) {
			var perform = (PerformProcedureStatement)node.CodeElement;
			CheckNotInTable(table, perform.Procedure, node);
			CheckNotInTable(table, perform.ThroughProcedure, node);
		}
		foreach(var child in node.Children) CheckNoPerform(table, child);
	}
	private void CheckNotInTable(SymbolTable table, SymbolReference symbol, Node node) {
		if (symbol == null) return;
		string message = "TCRFUN_NO_PERFORM_OF_ENCLOSING_PROGRAM";
		var found = table.GetSection(symbol.Name);
		if (found.Count > 0) DiagnosticUtils.AddError(node, message);
		else {
			var paragraphFounds = table.GetParagraph(symbol.Name);
			if (paragraphFounds.Count > 0) DiagnosticUtils.AddError(node, message);
		}
	}
}



/// <summary>
/// Checks the TypeCobol rules for Library
/// </summary>
public class LibraryChecker {

	public static void CheckLibrary([NotNull] ProcedureDivision procedureDivision) {
        //A procedure or a function cannot contains another procedure or function declaration
        //So we only need to check ProcedureDivision of Program
	    if ( !(procedureDivision.Parent is Program) )
	        return;


        //If the procedure division contains a PUBLIC procedure or function then it's considered as a "Library"
	    bool isLibrary = procedureDivision.Children.Any(c =>
		{
		    var f = c.CodeElement as FunctionDeclarationHeader;
		    return f!=null && f.Visibility == AccessModifier.Public;
		} );

	    if (isLibrary)
        {
	        foreach (var child in procedureDivision.Children)
            {
                //TCRFUN_ONLY_PARAGRAPH_AND_PUBLIC_FUNC_IN_LIBRARY
                if (!(child is Paragraph || child is FunctionDeclaration)) {
                    DiagnosticUtils.AddError(child.CodeElement == null ? procedureDivision : child, "Illegal non-function or paragraph item in library " + child.Name + " / " + child.ID);
                }
            }

		    var pdiv = procedureDivision.CodeElement as ProcedureDivisionHeader;

            //TCRFUN_LIBRARY_PROCEDURE_NO_USING 
            if (pdiv.UsingParameters != null && pdiv.UsingParameters.Count > 0)
			    DiagnosticUtils.AddError(procedureDivision, "Illegal "+pdiv.UsingParameters.Count+" USING in library PROCEDURE DIVISION.");
		}
	}
}


}

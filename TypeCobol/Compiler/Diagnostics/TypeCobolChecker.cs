using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeModel;
using System.Linq;
using System.Runtime.InteropServices;
using Analytics;
using Castle.Core.Internal;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics
{
    class ReadOnlyPropertiesChecker
    {
        private static string[] READONLY_DATATYPES = {"DATE",};

        public static void OnNode([NotNull] VariableWriter variableWriter, Node node)
        {
            var element = node.CodeElement as VariableWriter;
            if (element?.VariablesWritten != null)
                foreach (var pair in element.VariablesWritten)
                {
                    if (pair.Key == null) continue; // no receiving item
                    var receiving = node.GetDataDefinitionFromStorageAreaDictionary(pair.Key);
                    if (receiving == null)
                    {
                        continue; // ambiguity or not referenced; not my job
                    }

                    checkReadOnly(node, receiving);
                }
        }

        private static void checkReadOnly(Node node, [NotNull] Node receiving)
        {
            var rtype = receiving.Parent as DataDefinition;
            if (rtype == null || rtype.DataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85) return;
            foreach (var type in READONLY_DATATYPES)
            {
                if (type.Equals(rtype.DataType.Name, StringComparison.OrdinalIgnoreCase))
                    DiagnosticUtils.AddError(node, type + " properties are read-only");
            }
        }
    }

    class DataDefinitionChecker
    {
        public static void OnNode(Node node, DataDescriptionEntry dataEntry = null)
        {
            if (dataEntry == null && node is DataDefinition)
            {
                dataEntry = node.CodeElement as DataDescriptionEntry;
            }


            if (dataEntry?.Usage != null &&
                (dataEntry.Usage.Value == DataUsage.FloatingPoint || dataEntry.Usage.Value == DataUsage.LongFloatingPoint) &&
                dataEntry.Picture != null)
            {
                DiagnosticUtils.AddError(node,
                    "Variable with usage COMP-1 and COMP-2 cannot have a PICTURE", dataEntry);
            }
        }

    }

    class FunctionCallChecker
    {
        public static void OnNode(Node node)
        {
            var functionCaller = node as FunctionCaller;
            if (functionCaller == null || functionCaller.FunctionCall == null ||
                !functionCaller.FunctionCall.NeedDeclaration)
                return;

            if (functionCaller.FunctionDeclaration == null)
            {
                //Get Funtion by name and profile (matches on precise parameters)
                var parameterList = functionCaller.FunctionCall.AsProfile(node);
                var functionDeclarations =
                    node.SymbolTable.GetFunction(new URI(functionCaller.FunctionCall.FunctionName),
                    parameterList, functionCaller.FunctionCall.Namespace);

                string message;
                //There is one CallSite per function call
                //This is a call to a TypeCobol function or procedure with arguments
                if (node.CodeElement.CallSites.Count == 1 &&
                    node.CodeElement.CallSites[0].CallTarget.IsOrCanBeOnlyOfTypes(SymbolType.TCFunctionName))
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
                        message = string.Format("Same function '{0}' {1} declared '{2}' times",
                            functionCaller.FunctionCall.FunctionName, parameterList.GetSignature(),
                            functionDeclarations.Count);
                        DiagnosticUtils.AddError(node, message, MessageCode.ImplementationError);
                        return; //Do not continue the function/procedure is defined multiple times
                    }

                    var otherDeclarations =
                        node.SymbolTable.GetFunction(((ProcedureCall) functionCaller.FunctionCall).ProcedureName.URI,
                            null, functionCaller.FunctionCall.Namespace);

                    if (functionDeclarations.Count == 0 && otherDeclarations.Count == 0)
                    {
                        message = string.Format("Function not found '{0}' {1}",
                            functionCaller.FunctionCall.FunctionName,
                            parameterList.GetSignature());
                        DiagnosticUtils.AddError(node, message);
                        return; //Do not continue the function/procedure does not exists
                    }

                    if (otherDeclarations.Count > 1)
                    {
                        message = string.Format("No suitable function signature found for '{0}' {1}",
                            functionCaller.FunctionCall.FunctionName, parameterList.GetSignature());
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    functionDeclarations = otherDeclarations;
                }
                else
                {
                    //call to a TypeCobol function/procedure without arguments or to a Variable

                    var potentialVariables =
                        node.SymbolTable.GetVariablesExplicit(new URI(functionCaller.FunctionCall.FunctionName));

                    var potentialVariablesCount = potentialVariables.Count();
                    if (functionDeclarations.Count == 1 && potentialVariablesCount == 0)
                    {
                        functionCaller.FunctionDeclaration = functionDeclarations.First();
                        return; //Everything seems to be ok, lets continue on the next one
                    }

                    functionDeclarations =
                        node.SymbolTable.GetFunction(new URI(functionCaller.FunctionCall.FunctionName), null,
                            functionCaller.FunctionCall.Namespace);

                    if (potentialVariablesCount > 1)
                    {
                        //If there is more than one variable with the same name, it's ambiguous
                        message = string.Format("Call to '{0}'(no arguments) is ambigous. '{0}' is defined {1} times",
                            functionCaller.FunctionCall.FunctionName,
                            potentialVariables.Count() + functionDeclarations.Count);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count > 1 && potentialVariablesCount == 0)
                    {
                        message = string.Format("No suitable function signature found for '{0}(no arguments)'",
                            functionCaller.FunctionCall.FunctionName);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count >= 1 && potentialVariablesCount == 1)
                    {
                        message = string.Format("Warning: Risk of confusion in call of '{0}'",
                            functionCaller.FunctionCall.FunctionName);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count == 0 && potentialVariablesCount == 0)
                    {
                        message = string.Format("No function or variable found for '{0}'(no arguments)",
                            functionCaller.FunctionCall.FunctionName);
                        DiagnosticUtils.AddError(node, message);
                        return; //Do not continue the function/procedure does not exists
                    }

                    if (potentialVariablesCount == 1)
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
            var callerProfile = call.AsProfile(node);
            var callArgsCount = call.Arguments != null ? call.Arguments.Length : 0;
            if (callArgsCount > parameters.Count)
            {
                var m = string.Format("Function '{0}' only takes {1} parameter(s)", call.FunctionName,
                    parameters.Count);
                DiagnosticUtils.AddError(node, m);
            }

            if (callerProfile.InputParameters.Count != definition.Profile.InputParameters.Count
                || callerProfile.InoutParameters.Count != definition.Profile.InoutParameters.Count
                || callerProfile.OutputParameters.Count != definition.Profile.OutputParameters.Count)
            {
                var m = string.Format("No suitable function signature found for '{0}' {1}", call.FunctionName,
                    callerProfile.GetSignature());
                DiagnosticUtils.AddError(node, m);
            }

            for (int c = 0; c < parameters.Count; c++)
            {
                var expected = parameters[c];

                //Hack until we get a real concept of project to build all of the dependencies
                if (expected.CodeElement.UserDefinedDataType != null && expected.TypeDefinition == null)
                {
                    TypeCobolLinker.ResolveType(expected);
                    if (expected.TypeDefinition != null)
                    {
                        TypeCobolLinker.CheckCircularReferences(expected.TypeDefinition);
                    }
                }
                if (c < callArgsCount)
                {
                    //Omitted
                    if (call.Arguments[c].IsOmitted)
                    {
                        if (expected.IsOmittable)
                        {
                            continue;
                        }
                        else
                        {
                            DiagnosticUtils.AddError(node, "Omitted not allowed for this parameter");
                            return;
                        }
                    }
                    
                    
                    var actual = call.Arguments[c].StorageAreaOrValue;
                    if (actual.IsLiteral) continue; //TODO

                    var callArgName = actual.StorageArea.ToString();
                    var found = node.GetDataDefinitionFromStorageAreaDictionary(actual.StorageArea);
                   if (found == null)
                    {
                        continue;
                    }

                    var actualDataDefinition = found;

                    var actualSpecialRegister = actual.StorageArea as StorageAreaPropertySpecialRegister;
                    if (actualSpecialRegister != null)
                    {
                        var tokenType = actualSpecialRegister.SpecialRegisterName.TokenType;
                        if (tokenType == TokenType.LENGTH)
                        {
                            if (call is ProcedureCall)
                            {
                                ProcedureCall procedureCall = call as ProcedureCall;
                                if (procedureCall.OutputParameters.Contains(call.Arguments[c]))
                                {
                                    DiagnosticUtils.AddError(node, "LENGTH cannot be used as an output",
                                        actualSpecialRegister.SpecialRegisterName);
                                    continue;
                                }
                            }

                               // accepted format is "PIC [S]9(5..9) comp-5"
                            if (expected.PrimitiveDataType.Name != "Numeric" || expected.PhysicalLength != 4 || expected.Usage != DataUsage.NativeBinary)
                            {
                                DiagnosticUtils.AddError(node, "LENGTH can only be used as PIC S9(5..9) comp-5",
                                    actualSpecialRegister.SpecialRegisterName);
                                continue;
                            }
                        }
                        else if (tokenType == TokenType.ADDRESS && expected.Usage == DataUsage.Pointer)
                        {
                            if (!actualDataDefinition.IsFlagSet(Node.Flag.LinkageSectionNode) &&
                                call.Arguments[c].SharingMode.Value == ParameterSharingMode.ByReference)
                            {
                                DiagnosticUtils.AddError(node,
                                    "ADDRESS OF can only be used with a LINKAGE variable, or with a sharing mode BY CONTENT/BY VALUE",
                                    actualSpecialRegister.SpecialRegisterName);
                            }

                            continue;
                        }
                        else if (tokenType == TokenType.LINAGE_COUNTER)
                        {
                            //Do not know what to do : RFC
                            DiagnosticUtils.AddError(node, "LINAGE_COUNTER not allowed yet with procedure");
                            return;
                        }

                        continue; //If it's a special register we don't want to check more rules. 
                    }

                    

                    //TODO use SubscriptExpression and ReferenceModifier of the StorageArea to correct the type
                    //Ex: MyVar1(1:10) has a length of 10 and is of type Alphanumeric
                    //Ex: MyArray(1) only target one element of the array, so we need to get the type of this element.



                    //If the actual dataDefinition is a table occurence try to match it with subscripts
                    //If the actual dataDefinition is under a table occurence, then don't care about subscripts 
                    long actualMinOccurencesCount = actualDataDefinition.MinOccurencesCount;
                    long actualMaxOccurencesCount = actualDataDefinition.MaxOccurencesCount;
                    bool actualHasUnboundedNumberOfOccurences = actualDataDefinition.HasUnboundedNumberOfOccurences;
                    NumericVariable actualOccursDependingOn = actualDataDefinition.OccursDependingOn;
                    bool actualIsTableOccurence = actualDataDefinition.IsTableOccurence;

                    if (actualDataDefinition.IsTableOccurence)
                    {
                        var subscriptedStorageArea = actual.StorageArea as DataOrConditionStorageArea;
                        if (subscriptedStorageArea != null && subscriptedStorageArea.Subscripts.Count > 0)
                        {
                            //if there are subscripts


                            //Do not allow ALL
                            if (subscriptedStorageArea.Subscripts.Any(s => s.ALL != null))
                            {
                                DiagnosticUtils.AddError(node, "You cannot use ALL for procedure argument");
                                return;
                            }

                            actualMinOccurencesCount = 0;
                            actualMaxOccurencesCount = 0;
                            actualHasUnboundedNumberOfOccurences = false;
                            actualOccursDependingOn = null;
                            actualIsTableOccurence = false;
                        }
                    }
                    

                    //Cobol 85 Type will be checked with their picture
                    if (actualDataDefinition.DataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85 ||
                        expected.DataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85) 
                    {
                        if (actualDataDefinition.DataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85 ||
                            expected.DataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85)
                        {
                            var m = string.Format(
                                    "Function '{0}' expected parameter '{1}' of type {2} and received '{3}' of type {4} ",
                                    call.FunctionName, expected.Name, expected.DataType,
                                    callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.DataType);
                            DiagnosticUtils.AddError(node, m);

                            
                        }
                        else if (actualDataDefinition.DataType != expected.DataType)
                        {
                            TypeDefinition callerType = actualDataDefinition.TypeDefinition;
                            TypeDefinition calleeType = expected.TypeDefinition;
                            if (callerType != null && calleeType != null)
                            {
                                //Compare reference of TypeDefinition
                                if (callerType != calleeType)
                                {
                                    var m = string.Format(
                                        "Function '{0}' expected parameter '{1}' of type {2} and received '{3}' of type {4} ",
                                        call.FunctionName, calleeType.Name, calleeType.DataType,
                                        callArgName ?? string.Format("position {0}", c + 1), callerType.DataType);
                                    DiagnosticUtils.AddError(node, m);
                                }
                                //else
                                //DataType were not written exactly the same in the source code
                                //eg. we can have a type qualified with its program and the same type without the qualification, then DataType are not the same but TypeDefinition are
                                //So no error here, it's ok
                            }
                            else
                            {
                                //Ignore, it's an unknown DataType. It's already checked by TypeCobolLinker
                            }
                        }
                    }

                    if (actualDataDefinition.Picture != null && expected.Picture != null &&
                        actualDataDefinition.Picture.NormalizedValue != expected.Picture.NormalizedValue)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' with picture {2} and received '{3}' with picture {4}",
                                call.FunctionName, expected.Name, expected.Picture.Value,
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.Picture.Value);
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
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.IsJustified ? "justified" : "non-justified");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.IsGroupUsageNational != expected.IsGroupUsageNational)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                call.FunctionName, expected.Name,
                                expected.IsGroupUsageNational ? "national group-usage" : "non national group-usage",
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.IsGroupUsageNational
                                    ? "national group-usage"
                                    : "non national group-usage");
                        DiagnosticUtils.AddError(node, m);
                    }



                    //Array
                    if (actualIsTableOccurence != expected.IsTableOccurence)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' to {2} an array and received '{3}' which {4} an array",
                                call.FunctionName, expected.Name, expected.IsTableOccurence ? "be" : "be NOT",
                                actualDataDefinition.Name,
                                actualIsTableOccurence ? "is" : "is NOT ");
                        DiagnosticUtils.AddError(node, m);
                    }
                    else if (actualIsTableOccurence && expected.IsTableOccurence)
                    {
                        if (actualMinOccurencesCount != expected.MinOccurencesCount)
                        {
                            var m =
                                string.Format(
                                    "Function '{0}' expected parameter '{1}' to have at least {2} occurences and received '{3}' with a minimum of {4} occurences",
                                    call.FunctionName, expected.Name, expected.MinOccurencesCount,
                                    callArgName ?? string.Format("position {0}", c + 1), actualMinOccurencesCount);
                            DiagnosticUtils.AddError(node, m);
                        }

                        if (actualMaxOccurencesCount != expected.MaxOccurencesCount)
                        {
                            var m =
                                string.Format(
                                    "Function '{0}' expected parameter '{1}' to have at most {2} occurences and received '{3}' with a maximum of {4} occurences",
                                    call.FunctionName, expected.Name, expected.MaxOccurencesCount,
                                    callArgName ?? string.Format("position {0}", c + 1), actualMaxOccurencesCount);
                            DiagnosticUtils.AddError(node, m);
                        }
                    }

                    if (actualOccursDependingOn != expected.OccursDependingOn)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' occurs depending on ({2}) occurences and received '{3}' occurs depending on ({4})",
                               call.FunctionName, expected.Name, expected.OccursDependingOn,
                               callArgName ?? string.Format("position {0}", c + 1), actualOccursDependingOn);
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualHasUnboundedNumberOfOccurences != expected.HasUnboundedNumberOfOccurences)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                call.FunctionName, expected.Name,
                                expected.HasUnboundedNumberOfOccurences
                                    ? "has unbounded number of occurences"
                                    : "hasn't unbounded number of occurences",
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualHasUnboundedNumberOfOccurences
                                    ? "has unbounded number of occurences"
                                    : "hasn't unbounded number of occurences");
                        DiagnosticUtils.AddError(node, m);
                    }


                    if (actualDataDefinition.SignIsSeparate != expected.SignIsSeparate)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                call.FunctionName, expected.Name,
                                expected.HasUnboundedNumberOfOccurences
                                    ? "has unbounded number of occurences"
                                    : "hasn't unbounded number of occurences",
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.HasUnboundedNumberOfOccurences
                                    ? "has unbounded number of occurences"
                                    : "hasn't unbounded number of occurences");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.SignPosition != expected.SignPosition)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' with sign position {2} and received '{3}' with sign position {4}",
                                call.FunctionName, expected.Name,
                                expected.SignPosition == null ? "empty" : expected.SignPosition.ToString(),
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.SignPosition == null
                                    ? "empty"
                                    : actualDataDefinition.SignPosition.ToString());
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.IsSynchronized != expected.IsSynchronized)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                call.FunctionName, expected.Name,
                                expected.IsSynchronized ? "synchonized" : "not synchronized",
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.IsSynchronized ? "synchonized" : "not synchronized");
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
                        call.FunctionName, expected.Name, expected.DataType, expected.PhysicalLength);
                    DiagnosticUtils.AddError(node, m);
                }
            }
        }
    }

    class FunctionDeclarationTypeChecker
    {
        public static void OnCodeElement(FunctionDeclarationHeader function,
            CodeElementsParser.FunctionDeclarationHeaderContext context)
        {

            if (function.ActualType == FunctionType.Undefined)
            {
                DiagnosticUtils.AddError(function,
                    "Incompatible parameter clauses for " + ToString(function.UserDefinedType) + " \"" + function.Name +
                    "\"", context);
            }
            else if ((function.ActualType == FunctionType.Function &&
                      function.UserDefinedType == FunctionType.Procedure)
                     || (function.ActualType == FunctionType.Procedure &&
                         function.UserDefinedType == FunctionType.Function))
            {
                var message = "Symbol \"" + function.Name + "\" is defined as " + ToString(function.UserDefinedType)
                              + ", but parameter clauses describe a " + ToString(function.ActualType);
                DiagnosticUtils.AddError(function, message, context);
            }
        }

        private static string ToString(FunctionType type)
        {
		    if (type == FunctionType.Undefined) return "symbol";
		    if (type == FunctionType.Function) return "function";
		    if (type == FunctionType.Procedure) return "procedure";
		    return "function or procedure";
	    }
    }
    

    class FunctionDeclarationChecker
    {
     
        public static void OnNode(FunctionDeclaration functionDeclaration)
        {
            var header = functionDeclaration?.CodeElement;
            if (header == null) return; //not my job

            var filesection = functionDeclaration.Get<FileSection>("file");
            if (filesection != null) // TCRFUN_DECLARATION_NO_FILE_SECTION
            {
                
                DiagnosticUtils.AddError(filesection,
                    "Illegal FILE SECTION in function \"" + header.Name + "\" declaration");
            }

            CheckNoGlobalOrExternal(functionDeclaration.Get<DataDivision>("data-division"));
            CheckNoLinkageItemIsAParameter(functionDeclaration.Get<LinkageSection>("linkage"), header.Profile);

            CheckParameters(header.Profile, functionDeclaration);
            CheckNoPerform(functionDeclaration.SymbolTable.EnclosingScope, functionDeclaration);

            var headerNameURI = new URI(header.Name);
            var functions = functionDeclaration.SymbolTable.GetFunction(headerNameURI, functionDeclaration.Profile);
            if (functions.Count > 1)
                DiagnosticUtils.AddError(functionDeclaration,
                    "A function \"" + headerNameURI.Head + "\" with the same profile already exists in namespace \"" +
                    headerNameURI.Tail + "\".");


            //// Set a Warning if the formalized comment parameter is unknown or if the function parameter have no description
            if (header.FormalizedCommentDocumentation != null)
            {
                // Get the parameters inside the Formalized Comment that are not inside the function parameters
                var formComParamOrphan = header.FormalizedCommentDocumentation.Parameters.Keys.Except(
                    functionDeclaration.Profile.Parameters.Select(p => p.Name));

                // For each of them, place a warning on the orphan parameter definition (UserDefinedWord Token inside the FormCom)
                foreach (var orphan in formComParamOrphan)
                {
                    var tokens = header.ConsumedTokens.Where(t => t.TokenType == TokenType.UserDefinedWord && t.Text == orphan);
                    foreach (var token in tokens)
                    {
                        DiagnosticUtils.AddError(header,
                            "Parameter name does not match to any function parameter: " + orphan,
                            token, code: MessageCode.Warning);
                    }
                }

                // Get the parameters inside the function parameters that are not inside the Formalized Comment
                var sameParameters = functionDeclaration.Profile.Parameters.Where(p =>
                    header.FormalizedCommentDocumentation.Parameters.Keys.Contains(p.Name));

                var functionParamWithoutDesc = functionDeclaration.Profile.Parameters.Except(sameParameters);

                // For each of them, place a warning on the parameter definition
                foreach (var param in functionParamWithoutDesc)
                {
                    var token = param.CodeElement.ConsumedTokens.FirstOrDefault(t => t.TokenType == TokenType.UserDefinedWord);
                    if (token != null)
                    {
                        DiagnosticUtils.AddError(header,
                            "Parameter does not have any description inside the formalized comments: " + param.Name,
                            token, code: MessageCode.Warning);
                    }
                }
            }
        }

        private static void CheckNoGlobalOrExternal(DataDivision node)
        {
            if (node == null) return; // no DATA DIVISION
            foreach (var section in node.Children)
            {
                // "storage" sections
                foreach (var child in section.Children)
                {
                    var data = child.CodeElement as DataDescriptionEntry;
                    if (data == null) continue;
                    if (data.IsGlobal) // TCRFUN_DECLARATION_NO_GLOBAL
                        DiagnosticUtils.AddError(child, "Illegal GLOBAL clause in function data item.", data);
                }
            }
        }

        private static void CheckParameters([NotNull] ParametersProfile profile, Node node)
        {
            var parameters = profile.Parameters;
            foreach (var parameter in profile.InputParameters) CheckParameter(parameter, node);
            foreach (var parameter in profile.InoutParameters) CheckParameter(parameter, node);
            foreach (var parameter in profile.OutputParameters) CheckParameter(parameter, node);
            if (profile.ReturningParameter != null)
            {
                CheckParameter(profile.ReturningParameter, node);
                parameters.Add(profile.ReturningParameter);
            }

            foreach (
                    var duplicatedParameter in
                    parameters.GroupBy(p => p.Name).Where(g => g.Skip(1).Any()).SelectMany(g => g))
                //Group on parameter.Name //where group contains more than one item //reexpand to get all duplicated parameters 
            {
                DiagnosticUtils.AddError(node,
                    string.Format("Parameter with name '{0}' declared multiple times", duplicatedParameter.Name), duplicatedParameter);
            }


        }

        private static void CheckParameter([NotNull] ParameterDescriptionEntry parameter, Node node)
        {
            // TCRFUN_LEVEL_88_PARAMETERS
            if (parameter.LevelNumber?.Value != 1)
            {
                DiagnosticUtils.AddError(node,
                    "Condition parameter \"" + parameter.Name + "\" must be subordinate to another parameter.", parameter);
            }

            if (parameter.DataConditions != null)
            {
                foreach (var condition in parameter.DataConditions)
                {
                    if (condition.LevelNumber?.Value != 88)
                        DiagnosticUtils.AddError(node,
                            "Condition parameter \"" + condition.Name + "\" must be level 88.", condition);
                    if (condition.LevelNumber?.Value == 88 && parameter.DataType == DataType.Boolean)
                        DiagnosticUtils.AddError(node,
                            "The Level 88 symbol '" + parameter.Name +
                            "' cannot be declared under a BOOL typed symbol", condition);
                }
            }

        }

        /// <summary>TCRFUN_DECLARATION_NO_DUPLICATE_NAME</summary>
        /// <param name="node">LINKAGE SECTION node</param>
        /// <param name="profile">Parameters for original function</param>
        private static void CheckNoLinkageItemIsAParameter(LinkageSection node, ParametersProfile profile)
        {
            if (node == null) return; // no LINKAGE SECTION
            var linkage = new List<DataDefinition>();
            AddEntries(linkage, node);
            foreach (var description in linkage)
            {
                var used = Validate(profile.ReturningParameter, description.Name);
                if (used != null)
                {
                    AddErrorAlreadyParameter(description, description.Name);
                    continue;
                }

                used = GetParameter(profile.InputParameters, description.Name);
                if (used != null)
                {
                    AddErrorAlreadyParameter(description, description.Name);
                    continue;
                }

                used = GetParameter(profile.OutputParameters, description.Name);
                if (used != null)
                {
                    AddErrorAlreadyParameter(description, description.Name);
                    continue;
                }

                used = GetParameter(profile.InoutParameters, description.Name);
                if (used != null)
                {
                    AddErrorAlreadyParameter(description, description.Name);
                    continue;
                }
            }
        }

        private static void AddEntries(List<DataDefinition> linkage, LinkageSection node)
        {
            foreach (var definition in node.Children())
                AddEntries(linkage, definition);
        }

        private static void AddEntries([NotNull] List<DataDefinition> linkage, DataDefinition node)
        {
            linkage.Add(node);
            foreach (var child in node.Children())
                AddEntries(linkage, child);
        }

        private static ParameterDescriptionEntry GetParameter(IList<ParameterDescriptionEntry> parameters, string name)
        {
            if (name == null) return null;
            foreach (var p in parameters)
                if (Validate(p, name) != null)
                    return p;
            return null;
        }

        private static ParameterDescriptionEntry Validate(ParameterDescriptionEntry parameter, string name)
        {
            if (parameter != null && parameter.Name.Equals(name)) return parameter;
            return null;
        }

        private static void AddErrorAlreadyParameter([NotNull] Node node, [NotNull] string parameterName)
        {
            DiagnosticUtils.AddError(node, parameterName + " is already a parameter.");
        }

        private static void CheckNoPerform(SymbolTable table, [NotNull] Node node)
        {
            if (node is PerformProcedure)
            {
                var perform = (PerformProcedureStatement) node.CodeElement;
                CheckNotInTable(table, perform.Procedure, node);
                CheckNotInTable(table, perform.ThroughProcedure, node);
            }

            foreach (var child in node.Children) CheckNoPerform(table, child);
        }

        private static void CheckNotInTable(SymbolTable table, SymbolReference symbol, Node node)
        {
            if (symbol == null) return;
            string message = "TCRFUN_NO_PERFORM_OF_ENCLOSING_PROGRAM";
            var found = table.GetSection(symbol.Name);
            if (found.Count > 0) DiagnosticUtils.AddError(node, message, symbol);
            else
            {
                var paragraphFounds = table.GetParagraph(symbol.Name);
                if (paragraphFounds.Count > 0) DiagnosticUtils.AddError(node, message, symbol);
            }
        }
    }

    /// <summary>
/// Checks the TypeCobol rules for Library
/// </summary>
    public class LibraryChecker
    {
        public static void CheckLibrary([NotNull] ProcedureDivision procedureDivision)
        {
        //A procedure or a function cannot contains another procedure or function declaration
        //So we only need to check ProcedureDivision of Program
            if (!(procedureDivision.Parent is Program))
	        return;


        //If the procedure division contains a PUBLIC procedure or function then it's considered as a "Library"
	    bool isLibrary = procedureDivision.Children.Any(c =>
		{
		    var f = c.CodeElement as FunctionDeclarationHeader;
                return f != null && f.Visibility == AccessModifier.Public;
            });

	    if (isLibrary)
        {
	        bool firstParagraphChecked = false;
	        foreach (var child in procedureDivision.Children)
            {
                //TCRFUN_ONLY_PARAGRAPH_AND_PUBLIC_FUNC_IN_LIBRARY
                if (child is Paragraph)
                {
	                if (!firstParagraphChecked &&
	                    !child.Name.Equals("INIT-LIBRARY", StringComparison.InvariantCultureIgnoreCase))
                    {
                            DiagnosticUtils.AddError(child.CodeElement == null ? procedureDivision : child,
                                "First paragraph of a program which contains public procedure must be INIT-LIBRARY. Move paragraph " +
                                child.Name + " lower in the source.");
                    }

	                firstParagraphChecked = true;

	                continue; //A paragraph is always accepted as a child of ProcedureDivision
	            }

                //TCRFUN_ONLY_PARAGRAPH_AND_PUBLIC_FUNC_IN_LIBRARY
                if (!(child is FunctionDeclaration || child is Declaratives))
                {
                        DiagnosticUtils.AddError(child.CodeElement == null
                                ? (child is Sentence
                            ? (child.Children.FirstOrDefault(c => c.CodeElement != null) ?? procedureDivision)
                                    : procedureDivision)
                                : child,
                            "Inside a library only function declaration or declaratives are allowed " + child.Name +
                            " / " + child.ID);
                    }
            }

		    var pdiv = procedureDivision.CodeElement;

            //TCRFUN_LIBRARY_PROCEDURE_NO_USING 
            if (pdiv?.UsingParameters != null && pdiv.UsingParameters.Count > 0)
                    DiagnosticUtils.AddError(procedureDivision,
                        "Illegal " + pdiv.UsingParameters.Count + " USING in library PROCEDURE DIVISION.");
		}
	}
}

    public class SetStatementChecker
    {
        public static void CheckStatement(Node node)
        {
            var statement = node.CodeElement as SetStatementForIndexes;
            if (statement != null)
            {
                // Check receivers (incremented) 
                var receivers = node?.StorageAreaWritesDataDefinition?.Values;
                if (receivers == null)
                    return;
                bool containsPointers = false;
                bool allArePointers = true;
                foreach (var receiver in receivers)
                {
                    if (receiver.Usage == DataUsage.Pointer)
                    {
                        containsPointers = true;
                        var levelNumber = (receiver.CodeElement).LevelNumber;
                        if (levelNumber != null && levelNumber.Value > 49)
                        {
                            DiagnosticUtils.AddError(node,
                                "Only pointer declared in level 01 to 49 can be use in instructions SET UP BY and SET DOWN BY.", receiver.CodeElement); 
                        }

                        if (receiver.Name.Length > 22)
                        {
                            DiagnosticUtils.AddError(node,
                                "Pointer name '" + receiver.Name + "' is over 22 characters.", receiver.CodeElement);
                        }

                        if (receiver.IsInsideCopy())
                        {
                            DiagnosticUtils.AddError(node,
                                "Pointer '" + receiver.Name + "' belongs to a copy.");
                        }

                        receiver.SetFlag(Node.Flag.NodeisIncrementedPointer, true);
                    }
                    else
                        allArePointers = false; 

                        // Do note break here because it can be all indexes wich is correct or a pointer as last receiver wich is not
                }

                if (allArePointers)
                    node.SetFlag(Node.Flag.NodeContainsPointer, true);
                // If the receivers contains at least one Pointer, they must all be pointer
                else if (containsPointers)
                    DiagnosticUtils.AddError(node,
                        "[Set [pointer1, pointer2 ...] UP|DOWN BY n] only support pointers.");
                
                // Check sender (increment)
                int outputResult; // not used
                if (!int.TryParse(statement.SendingVariable.ToString(), out outputResult))
                {
                    // Not an integer
                    var variable =
                        node.GetDataDefinitionForQualifiedName(new URI(statement.SendingVariable.ToString()));
                    if (variable == null || variable.DataType.Name != "Numeric")
                    {
                        // Not an Variable or a notNumeric variable
                        if (statement.SendingVariable.ArithmeticExpression == null)
                            // Not an arithmetic expressions
                            DiagnosticUtils.AddError(node,
                                "Increment only support integer values, numeric variables and arithmetic expressions");
                    }
                }
            }
        }
    }

    public class ProgramChecker
    {
        public static void OnNode(Program node)
        {
            node.SetFlag(Node.Flag.MissingEndProgram, !(node.Children.LastOrDefault() is End));

            if (node.IsFlagSet(Node.Flag.MissingEndProgram))
            {
                DiagnosticUtils.AddError(node,
                    "\"END PROGRAM\" is missing.", MessageCode.Warning);
            }

        }
    }


    public class GlobalStorageSectionChecker
    {
        public static void OnNode([NotNull] GlobalStorageSection globalStorageSection)
        {
            //Check if GlobalStorageSection is declared in main program Rule - GLOBALSS_ONLY_IN_MAIN 
            if (!globalStorageSection.GetProgramNode().IsMainProgram)
                DiagnosticUtils.AddError(globalStorageSection,
                    "GLOBAL-STORAGE SECTION is only authorized in the main program of this source file.");

            //Check every GlobalStorageSection DataDefinition (children)
            foreach (var child in globalStorageSection.Children)
            {
                CheckGlobalStorageChildren(child);
            }
        }

        private static void CheckGlobalStorageChildren(Node node)
        {
            var dataDefinition = node as DataDefinition;
            if (dataDefinition == null) return;

            //Check variable LevelNumber Rule - GLOBALSS_LIMIT48 
            var data = dataDefinition.CodeElement;
            if (data?.LevelNumber != null && data.LevelNumber.Value == 77)
                DiagnosticUtils.AddError(node,
                    "Level 77 is forbidden in global-storage section.", data);

            //Check variable no Global / External keyword 
            // Rules : - GLOBALSS_NO_GLOBAL_KEYWORD - GLOBALSS_NO_EXTERNAL 
            var dataDescription = dataDefinition.CodeElement as DataDescriptionEntry;
            if (dataDescription != null)
            {
                if (dataDescription.IsGlobal) // GLOBALSS_NO_GLOBAL_KEYWORD 
                    DiagnosticUtils.AddError(dataDefinition, "Illegal GLOBAL clause in GLOBAL-STORAGE SECTION.", dataDescription);
                if (dataDescription.IsExternal) //GLOBALSS_NO_EXTERNAL
                    DiagnosticUtils.AddError(dataDefinition, "Illegal EXTERNAL clause in GLOBAL-STORAGE SECTION.", dataDescription);
            }


            if (node.Children.Count > 0)
            {
                foreach (var child in node.Children)
                {
                    CheckGlobalStorageChildren(child);
                }
            }
        }


    }

}
        


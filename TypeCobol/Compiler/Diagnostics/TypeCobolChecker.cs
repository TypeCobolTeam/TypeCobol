using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Diagnostics
{
    class ReadOnlyPropertiesChecker
    {
        private static readonly string[] READONLY_DATATYPES = { "DATE" };

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
                var functionName = functionCaller.FunctionCall.FunctionName;
                if (functionName == null)
                {
                    DiagnosticUtils.AddError(node, "Invalid call: function name is missing.");
                    //Cannot attempt function resolution, abort
                    return;
                }

                var callProfile = functionCaller.FunctionCall.BuildProfile(node);

                //Get Function by name and profile (matches on precise parameters)
                var functionDeclarations =
                    node.SymbolTable.GetFunction(new URI(functionName),
                    callProfile, functionCaller.FunctionCall.Namespace);

                string message;
                //There is one CallSite per function call
                //This is a call to a TypeCobol function or procedure with arguments
                if (node.CodeElement.CallSites.Count == 1
                    && node.CodeElement.CallSites[0].CallTarget != null
                    && node.CodeElement.CallSites[0].CallTarget.IsOrCanBeOnlyOfTypes(SymbolType.TCFunctionName))
                {
                    if (functionDeclarations.Count == 1)
                    {
                        functionCaller.FunctionDeclaration = functionDeclarations.First();
                        Check(node, functionCaller.FunctionCall, callProfile, functionCaller.FunctionDeclaration);
                        return; //Everything seems to be ok, lets continue on the next one
                    }

                    //Another checker should check if function declaration is not duplicated
                    if (functionDeclarations.Count > 0)
                    {
                        message = string.Format("Same function '{0}' {1} declared '{2}' times",
                            functionName, callProfile.GetSignature(),
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
                            functionName,
                            callProfile.GetSignature());
                        DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
                        return; //Do not continue the function/procedure does not exists
                    }

                    if (otherDeclarations.Count > 1)
                    {
                        message = string.Format("No suitable function signature found for '{0}' {1}",
                            functionName, callProfile.GetSignature());
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    functionDeclarations = otherDeclarations;
                }
                else
                {
                    //call to a TypeCobol function/procedure without arguments or to a Variable

                    var potentialVariables =
                        node.SymbolTable.GetVariablesExplicit(new URI(functionName));

                    var potentialVariablesCount = potentialVariables.Count();
                    if (functionDeclarations.Count == 1 && potentialVariablesCount == 0)
                    {
                        functionCaller.FunctionDeclaration = functionDeclarations.First();
                        return; //Everything seems to be ok, lets continue on the next one
                    }

                    functionDeclarations =
                        node.SymbolTable.GetFunction(new URI(functionName), null,
                            functionCaller.FunctionCall.Namespace);

                    if (potentialVariablesCount > 1)
                    {
                        //If there is more than one variable with the same name, it's ambiguous
                        message = string.Format("Call to '{0}'(no arguments) is ambigous. '{0}' is defined {1} times",
                            functionName,
                            potentialVariablesCount + functionDeclarations.Count);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count > 1 && potentialVariablesCount == 0)
                    {
                        message = string.Format("No suitable function signature found for '{0}(no arguments)'",
                            functionName);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count >= 1 && potentialVariablesCount == 1)
                    {
                        message = string.Format("Warning: Risk of confusion in call of '{0}'",
                            functionName);
                        DiagnosticUtils.AddError(node, message);
                        return;
                    }

                    if (functionDeclarations.Count == 0 && potentialVariablesCount == 0)
                    {
                        message = string.Format("No function or variable found for '{0}'(no arguments)",
                            functionName);
                        DiagnosticUtils.AddError(node, message);
                        return; //Do not continue the function/procedure does not exists
                    }

                    if (potentialVariablesCount == 1)
                        return; //Stop here, it's a standard Cobol call
                }


                functionCaller.FunctionDeclaration = functionDeclarations[0];
                //If function is not ambigous and exists, lets check the parameters
                Check(node, functionCaller.FunctionCall, callProfile, functionCaller.FunctionDeclaration);
            }
        }

        private static void Check(Node node, [NotNull] FunctionCall call, [NotNull] IProfile callProfile, [NotNull] FunctionDeclaration definition)
        {
            var functionName = call.FunctionName;
            System.Diagnostics.Debug.Assert(functionName != null);//already checked in OnNode method

            var parameters = definition.Profile.Parameters;
            var callArgsCount = call.Arguments != null ? call.Arguments.Length : 0;
            if (callArgsCount > parameters.Count)
            {
                var m = string.Format("Function '{0}' only takes {1} parameter(s)", functionName,
                    parameters.Count);
                DiagnosticUtils.AddError(node, m);
            }

            if (callProfile.Inputs.Count != definition.Profile.InputParameters.Count
                || callProfile.Inouts.Count != definition.Profile.InoutParameters.Count
                || callProfile.Outputs.Count != definition.Profile.OutputParameters.Count)
            {
                var m = string.Format("No suitable function signature found for '{0}' {1}", functionName,
                    callProfile.GetSignature());
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

                    var found = node.GetDataDefinitionFromStorageAreaDictionary(actual.StorageArea);
                    if (found == null)
                    {
                        continue;
                    }

                    var callArgName = actual.StorageArea.ToString();
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
                            if (expected.PrimitiveDataType != DataType.Numeric || expected.PhysicalLength != 4 || expected.Usage != DataUsage.NativeBinary)
                            {
                                DiagnosticUtils.AddError(node, "LENGTH can only be used as PIC S9(5..9) comp-5",
                                    actualSpecialRegister.SpecialRegisterName);
                                continue;
                            }
                        }
                        // Here we manage only DataUsage = POINTER (and not POINTER-32)
                        // because POINTER-32 is not supported in TypeCobol specific syntax "DECLARE function/procedure"
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
                        if (subscriptedStorageArea != null && subscriptedStorageArea.Subscripts.Length > 0)
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
                                    functionName, expected.Name, expected.DataType,
                                    callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.DataType);
                            DiagnosticUtils.AddError(node, m);

                            
                        }
                        else
                        {
                            TypeDefinition callerType = actualDataDefinition.TypeDefinition;
                            TypeDefinition calleeType = expected.TypeDefinition;
                            if (callerType != null && calleeType != null)
                            {
                                //Compare TypeDefinitions
                                if (!callerType.Equals(calleeType))
                                {
                                    var m = string.Format(
                                        "Function '{0}' expected parameter '{1}' of type {2} and received '{3}' of type {4} ",
                                        functionName, expected.Name, calleeType.QualifiedName,
                                        callArgName ?? string.Format("position {0}", c + 1), callerType.QualifiedName);
                                    DiagnosticUtils.AddError(node, m);
                                }
                                //else
                                //Same TypeDefinition it's ok.
                                //Note that DataType may differ: we can have a type qualified with its program and the same type without the qualification,
                                //in that case DataType are not the same but TypeDefinition are
                            }
                            //else
                            //Ignore, it's an unknown DataType. It's already checked by TypeCobolLinker
                        }
                    }

                    if (actualDataDefinition.Picture != null && expected.Picture != null &&
                        actualDataDefinition.Picture.NormalizedValue != expected.Picture.NormalizedValue)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' with picture {2} and received '{3}' with picture {4}",
                                functionName, expected.Name, expected.Picture.Value,
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.Picture.Value);
                        DiagnosticUtils.AddError(node, m);
                    }
                    

                    if (actualDataDefinition.Usage != expected.Usage)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' of usage {2} and received '{3}' of usage {4}",
                                functionName, expected.Name, expected.Usage,
                                callArgName ?? string.Format("position {0}", c + 1), actualDataDefinition.Usage);
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.IsJustified != expected.IsJustified)
                    {
                        var m =
                            string.Format(
                                "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                functionName, expected.Name, expected.IsJustified ? "justified" : "non-justified",
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.IsJustified ? "justified" : "non-justified");
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualDataDefinition.GroupUsage != expected.GroupUsage)
                    {
                        var m = string.Format("Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                functionName,
                                expected.Name,
                                DescribeGroupUsage(expected.GroupUsage),
                                callArgName ?? $"position {c + 1}",
                                DescribeGroupUsage(actualDataDefinition.GroupUsage));
                        DiagnosticUtils.AddError(node, m);

                        static string DescribeGroupUsage(DataUsage? groupUsage)
                        {
                            if (!groupUsage.HasValue)
                                return "non national, non UTF-8 group-usage";

                            return $"{(groupUsage.Value == DataUsage.National ? "national" : "UTF-8")} group-usage";
                        }
                    }



                    //Array
                    if (actualIsTableOccurence != expected.IsTableOccurence)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' to {2} an array and received '{3}' which {4} an array",
                                functionName, expected.Name, expected.IsTableOccurence ? "be" : "be NOT",
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
                                    functionName, expected.Name, expected.MinOccurencesCount,
                                    callArgName ?? string.Format("position {0}", c + 1), actualMinOccurencesCount);
                            DiagnosticUtils.AddError(node, m);
                        }

                        if (actualMaxOccurencesCount != expected.MaxOccurencesCount)
                        {
                            var m =
                                string.Format(
                                    "Function '{0}' expected parameter '{1}' to have at most {2} occurences and received '{3}' with a maximum of {4} occurences",
                                    functionName, expected.Name, expected.MaxOccurencesCount,
                                    callArgName ?? string.Format("position {0}", c + 1), actualMaxOccurencesCount);
                            DiagnosticUtils.AddError(node, m);
                        }
                    }

                    if (actualOccursDependingOn != expected.OccursDependingOn)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' occurs depending on ({2}) occurences and received '{3}' occurs depending on ({4})",
                               functionName, expected.Name, expected.OccursDependingOn,
                               callArgName ?? string.Format("position {0}", c + 1), actualOccursDependingOn);
                        DiagnosticUtils.AddError(node, m);
                    }

                    if (actualHasUnboundedNumberOfOccurences != expected.HasUnboundedNumberOfOccurences)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                functionName, expected.Name,
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
                                functionName, expected.Name,
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
                                functionName, expected.Name,
                                expected.SignPosition == null ? "empty" : expected.SignPosition.ToString(),
                                callArgName ?? string.Format("position {0}", c + 1),
                                actualDataDefinition.SignPosition == null
                                    ? "empty"
                                    : actualDataDefinition.SignPosition.ToString());
                        DiagnosticUtils.AddError(node, m);
                    }

                    bool actualIsSynchronized = actualDataDefinition.Synchronized != null;
                    bool expectedIsSynchronized = expected.Synchronized != null;
                    if (actualIsSynchronized != expectedIsSynchronized)
                    {
                        var m =
                           string.Format(
                               "Function '{0}' expected parameter '{1}' {2} and received '{3}' {4}",
                                functionName,
                                expected.Name,
                                SyncToString(expected.Synchronized),
                                callArgName ?? string.Format("position {0}", c + 1),
                                SyncToString(actualDataDefinition.Synchronized));
                        DiagnosticUtils.AddError(node, m);

                        string SyncToString(SyncAlignment? syncAlignment)
                        {
                            return syncAlignment.HasValue
                                ? "synchronized" + (syncAlignment.Value == SyncAlignment.None ? string.Empty : $" ({syncAlignment.Value})")
                                : "not synchronized";
                        }
                    }

                    if (actualDataDefinition.ObjectReferenceClass != expected.ObjectReferenceClass)
                    {
                        var m =
                          string.Format(
                              "Function '{0}' expected parameter '{1}' and received '{2}' with wrong object reference.",
                              functionName, expected.Name, callArgName ?? string.Format("position {0}", c + 1));
                        DiagnosticUtils.AddError(node, m);
                    }
                }
                else
                {
                    var m = string.Format("Function '{0}' is missing parameter '{1}' of type {2} and length {3}",
                        functionName, expected.Name, expected.DataType, expected.PhysicalLength);
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
            CheckNoUsingProcedureDiv(functionDeclaration.Get<ProcedureDivision>("procedure-division"));
            CheckParameters(header.Profile, functionDeclaration);

            var headerNameURI = new URI(header.Name);
            var functions = functionDeclaration.SymbolTable.GetFunction(headerNameURI, functionDeclaration.Profile);
            if (functions.Count > 1)
            {
                DiagnosticUtils.AddError(functionDeclaration,
                    "A function \"" + headerNameURI.Head + "\" with the same profile already exists in namespace \"" +
                    headerNameURI.Tail + "\".", header.FunctionName, MessageCode.SemanticTCErrorInParser);
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
            if (profile.ReturningParameter != null)
            {
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

        /// <summary>
        /// TCRFUN_DECLARATION_NO_USING
        /// </summary>
        private static void CheckNoUsingProcedureDiv(ProcedureDivision node)
        {
            if (node == null) return; //No procedure division

            if (node.CodeElement.UsingParameters != null && node.CodeElement.UsingParameters.Count > 0)
                DiagnosticUtils.AddError(node, "TypeCobol procedure cannot declare parameters on its procedure division.");
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
                                "First paragraph of a program which contains public procedure must be INIT-LIBRARY. Paragraph " + child.Name + " is not allowed at this location.");
                    }

                    firstParagraphChecked = true;

                    continue; //A paragraph is always accepted as a child of ProcedureDivision
                }

                //TCRFUN_ONLY_PARAGRAPH_AND_PUBLIC_FUNC_IN_LIBRARY
                if (!(child is FunctionDeclaration || child is Declaratives))
                {
                    Node node = child.CodeElement == null
                        ? (child is Sentence
                            ? (child.Children.FirstOrDefault(c => c.CodeElement != null) ?? procedureDivision)
                            : procedureDivision)
                        : child;
                    if (firstParagraphChecked)
                    {
                        // this case corresponds to SECTION declarations or statements not inside a paragraph
                        DiagnosticUtils.AddError(node,
                            "A program which contains public procedure cannot contain section or statement not under a paragraph.");
                    }
                    else
                    {
                        DiagnosticUtils.AddError(node,
                            "A program which contains public procedure must have INIT-LIBRARY as first paragraph.");
                    }
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
                var receivers = node.StorageAreaWritesDataDefinition?.Values;
                if (receivers == null)
                    return;
                bool containsPointers = false;
                bool allArePointers = true;
                foreach (var receiver in receivers)
                {
                    if (receiver.Usage == DataUsage.Pointer || receiver.Usage == DataUsage.Pointer32)
                    {
                        containsPointers = true;
                        System.Diagnostics.Debug.Assert(receiver.CodeElement != null);
                        var levelNumber = receiver.CodeElement.LevelNumber;
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
                if (statement.SendingVariable != null)
                {
                    if (!int.TryParse(statement.SendingVariable.ToString(), out var _))
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

            if (node.StorageAreaWritesDataDefinition != null && node.CodeElement is SetStatementForConditions setConditions && setConditions.IsSendingValueFalse)
            {
                // Statement is a SET TO FALSE: check whether it mixes variables of type BOOL and Level 88
                bool hasBool = false;
                bool hasLevel88 = false;
                string errorMessage = "Mixing TypeCobol BOOL variables with Level 88 in the same \"SET\" statement is not allowed. Consider splitting it into 2 separate statements.";
                foreach (var condition in setConditions.Conditions)
                {
                    if (condition.StorageArea != null && node.StorageAreaWritesDataDefinition.TryGetValue(condition.StorageArea, out var dataCondition))
                    {
                        if (dataCondition?.CodeElement.Type == CodeElementType.DataConditionEntry)
                        {
                            hasLevel88 = true;
                            if (hasBool)
                            {
                                DiagnosticUtils.AddError(node, errorMessage);
                                break;
                            }
                        }
                        else if (dataCondition?.CodeElement is DataDescriptionEntry dataDescriptionEntry && dataDescriptionEntry.DataType == DataType.Boolean)
                        {
                            hasBool = true;
                            if (hasLevel88)
                            {
                                DiagnosticUtils.AddError(node, errorMessage);
                                break;
                            }
                        }
                    }
                }

                // Set a flag to remember this check
                node.SetFlag(Node.Flag.IsTypeCobolSetToFalse, hasBool && !hasLevel88);
            }
        }
    }

    public class GlobalStorageSectionChecker
    {
        public static void OnNode([NotNull] GlobalStorageSection globalStorageSection)
        {
            //Check if GlobalStorageSection is declared in main program Rule - GLOBALSS_ONLY_IN_MAIN 
            if (globalStorageSection.IsFlagSet(Node.Flag.InsideProcedure) || !globalStorageSection.GetProgramNode().IsMainProgram)
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

            if (data?.LevelNumber != null && data.LevelNumber.Value == 49)
                DiagnosticUtils.AddError(node,
                    "Data declaration in global-storage section cannot be level 49", data);

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

    /// <summary>
    /// Contains methods to check syntax of formalized comments.
    /// </summary>
    internal static class FormalizedCommentsChecker
    {
        public static void CheckFunctionComments(FunctionDeclaration functionDeclaration)
        {
            //For TC procedures, comments are located directly above function declaration and forbidden on procedure division
            var procedureDivision = GetProcedureDivision(functionDeclaration);
            if (procedureDivision?.CodeElement.FormalizedCommentDocumentation != null)
            {
                DiagnosticUtils.AddError(procedureDivision,
                    "Formalized Comments can be placed above Procedure Division only for Programs",
                    procedureDivision.CodeElement,
                    MessageCode.ErrorFormalizedCommentMissplaced);
            }

            var comments = functionDeclaration.CodeElement.FormalizedCommentDocumentation;
            if (comments == null) return;

            //Check for duplicate tags
            CheckDuplicateFields(functionDeclaration);

            //Check matching between declared params and documented params
            var declaredParams = functionDeclaration.Profile.Parameters.Select(p => p.Name).ToArray();
            var documentedParams = comments.Parameters.Keys;
            CheckParameterMatching(functionDeclaration, declaredParams, documentedParams, "function");
        }

        public static void CheckProgramComments(Program program)
        {
            //For regular Cobol programs, comments are above the procedure division
            var procedureDivision = GetProcedureDivision(program);
            var comments = procedureDivision?.CodeElement.FormalizedCommentDocumentation;
            if (comments == null) return;

            //Check for duplicate tags
            CheckDuplicateFields(procedureDivision);

            //Check matching between declared params and documented params
            var usingParameters = procedureDivision.CodeElement.UsingParameters;
            var declaredParams = usingParameters != null
                ? usingParameters.Select(p => p.StorageArea.SymbolReference?.Name).ToArray()
                : Array.Empty<string>();
            var documentedParams = comments.Parameters.Keys;
            CheckParameterMatching(procedureDivision, declaredParams, documentedParams, "program");
        }

        public static void CheckTypeComments(TypeDefinition typeDefinition)
        {
            var comments = typeDefinition.CodeElement.FormalizedCommentDocumentation;
            if (comments == null) return;

            //Check for duplicate tags
            CheckDuplicateFields(typeDefinition);

            // Add a warning if a parameters field is set inside the formalized comment
            if (comments.Parameters.Any())
            {
                var token = typeDefinition.CodeElement.ConsumedTokens.First(t => t.TokenType == TokenType.FORMALIZED_COMMENTS_PARAMETERS);
                DiagnosticUtils.AddError(typeDefinition, "Type Definition does not support Parameters field", token, code: MessageCode.Warning);
            }
        }

        private static ProcedureDivision GetProcedureDivision(Node node) => node.Children.OfType<ProcedureDivision>().FirstOrDefault();

        private static void CheckDuplicateFields(Node commentsOwner)
        {
            var tokenGroups = commentsOwner.CodeElement.ConsumedTokens.GroupBy(t => t.TokenType);
            foreach (var tokenGroup in tokenGroups)
            {
                //Check all tags, so look for all formalized comments TokenTypes except START, STOP and VALUE.
                if (tokenGroup.Key >= TokenType.FORMALIZED_COMMENTS_DESCRIPTION && tokenGroup.Key <= TokenType.FORMALIZED_COMMENTS_TODO && tokenGroup.Count() > 1)
                {
                    foreach (var token in tokenGroup)
                    {
                        DiagnosticUtils.AddError(commentsOwner,
                            "Formalized comment field is declared more than once : " + token.Text, token,
                            code: MessageCode.Warning);
                    }
                }
            }
        }

        private static void CheckParameterMatching(Node commentsOwner, ICollection<string> declaredParams, ICollection<string> documentedParams, string context)
        {
            // User-defined words of the CE defining the comments, this will be used to position the diagnostics
            var userDefinedWords = commentsOwner.CodeElement.ConsumedTokens.Where(t => t.TokenType == TokenType.UserDefinedWord).ToArray();

            // Get the parameters inside the Formalized Comment that are not inside the program/function parameters
            var formComParamOrphan = documentedParams.Except(declaredParams);

            // For each of them, place a warning on the orphan parameter definition (UserDefinedWord Token inside the FormCom)
            AddDiagnostics(formComParamOrphan, "Parameter name does not match to any " + context + " parameter: ");

            // Get the parameters inside the program/function parameters that are not inside the Formalized Comment
            var programParamWithoutDesc = declaredParams.Except(documentedParams);

            // For each of them, place a warning on the parameter definition
            AddDiagnostics(programParamWithoutDesc, "Parameter does not have any description inside the formalized comments: ");

            void AddDiagnostics(IEnumerable<string> orphans, string message)
            {
                foreach (var orphan in orphans)
                {
                    foreach (var token in userDefinedWords)
                    {
                        if (token.Text == orphan)
                        {
                            DiagnosticUtils.AddError(commentsOwner, message + orphan, token, code: MessageCode.Warning);
                        }
                    }
                }
            }
        }
    }
}

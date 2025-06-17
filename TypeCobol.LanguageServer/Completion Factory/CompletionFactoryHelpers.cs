using System.Text;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.SignatureHelper;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal static class CompletionFactoryHelpers
    {
        /// <summary>
        /// Help to resolve procedure name inside consumed tokens.
        /// Will return a string containing only a proc name or an entire qualified name for the procedure (depending on the given tokens)
        /// </summary>
        /// <param name="consumedTokens"></param>
        /// <returns></returns>
        public static string GetProcedureNameFromTokens(List<Token> consumedTokens)
        {
            //Get procedure name or qualified name
            return string.Join(".", consumedTokens
                .Skip(1) //Skip the CALL token
                .TakeWhile(t => t.TokenType != TokenType.INPUT
                                && t.TokenType != TokenType.OUTPUT
                                && t.TokenType != TokenType.IN_OUT) // Take tokens until keyword found
                .Where(t => t.TokenType == TokenType.UserDefinedWord)
                .Select(t => t.Text));
        }

        public static List<CompletionItem> CreateCompletionItemsForType(IEnumerable<TypeDefinition> types, Node node, bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            foreach (var type in types)
            {
                bool typeIsPublic = false;
                bool typeIsIntrinsic = type.IsFlagSet(Node.Flag.NodeIsIntrinsic);
                if (enablePublicFlag)
                {
                    typeIsPublic = type.CodeElement?.Visibility == AccessModifier.Public
                                   && !(type.GetProgramNode() == node.GetProgramNode()  //Ignore public if type is in the current program
                                    || typeIsIntrinsic); //Ignore public if type is in intrinsic
                }
                    

                var typeDisplayName = typeIsPublic ? type.VisualQualifiedName.ToString() : type.Name;
                var completionItem = new CompletionItem() { label = typeDisplayName };

                if (!(node is FunctionDeclaration))
                    if (typeIsPublic)
                    {
                        completionItem.insertText =
                            //Check if last element is of type PeriodSperator (ie : a dot), so the completion does not make a duplicate
                            (node.CodeElement.ConsumedTokens.Last().TokenType == TokenType.PeriodSeparator)
                                ? $"{type.VisualQualifiedName.Tail}::{type.VisualQualifiedName.Head}" 
                                : $"{type.VisualQualifiedName.Tail}::{type.VisualQualifiedName.Head}.";
                    }
                    else
                    {
                        completionItem.insertText =
                            //Check if last element is of type PeriodSperator (ie : a dot), so the completion does not make a duplicate
                            (node.CodeElement.ConsumedTokens.Last().TokenType == TokenType.PeriodSeparator)
                                ? completionItem.insertText = type.Name
                                : completionItem.insertText = type.Name + ".";
                    }

                completionItem.kind = typeIsIntrinsic ? CompletionItemKind.IntrinsicType : CompletionItemKind.Class;
                completionItems.Add(completionItem);
            }
            return completionItems;
        }

        public static List<CompletionItem> CreateCompletionItemsForProcedures(IEnumerable<FunctionDeclaration> procedures, Node node, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary,  bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            Token callToken = node.CodeElement.ConsumedTokens.First(t => t.TokenType == TokenType.CALL);
            Dictionary<ParameterDescription.PassingTypes, string> paramWithCase = GetParamsUsingMatchingCase(callToken);

            string inputLabel = paramWithCase[ParameterDescription.PassingTypes.Input];
            string inoutLabel = paramWithCase[ParameterDescription.PassingTypes.InOut];
            string outputLabel = paramWithCase[ParameterDescription.PassingTypes.Output];

            foreach (var proc in procedures)
            {
                string inputParams = null, outputParams = null, inoutParams = null;

                if (proc.Profile != null)
                {
                    inputParams = FormatParameters(inputLabel, proc.Profile.InputParameters);
                    inoutParams = FormatParameters(inoutLabel, proc.Profile.InoutParameters);
                    outputParams = FormatParameters(outputLabel, proc.Profile.OutputParameters);

                    static string FormatParameters(string label, IList<ParameterDescription> parameters)
                    {
                        if (parameters == null || parameters.Count == 0)
                        {
                            return null;
                        }
                        var parameterList = string.Join(", ", parameters.Select(p => $"{p.DataName}({p.DataType.Name})"));
                        return $"{label} {parameterList}";
                    }
                }
                bool procIsPublic = false;
                if (enablePublicFlag)
                    procIsPublic = proc.CodeElement.Visibility == AccessModifier.Public
                                   &&
                                   !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Program)
                                         .Functions.Values.Any(t => t.Contains(proc))
                                     //Ignore public if proc is in the current program
                                     || proc.IsFlagSet(Node.Flag.NodeIsIntrinsic)); //Ignore public if proc is in intrinsic;
                var procDisplayName = procIsPublic ? proc.VisualQualifiedName.ToString() : proc.Name;
                var completionItem = new CompletionItem();
                // Completion item's label is procedure name followed by the not null parameters
                var builder = new StringBuilder(procDisplayName);
                AppendIf(inputParams, inputParams != null);
                AppendIf(inoutParams, inoutParams != null);
                AppendIf(outputParams, outputParams != null);
                completionItem.label = builder.ToString();
                builder.Clear();
                // Completion item's insertText is (qualified) procedure name followed by the 1st meaningful parameter label
                builder.Append(procIsPublic ? $"{proc.VisualQualifiedName.Tail}::{proc.VisualQualifiedName.Head}" : proc.Name);
                if (!AppendIf(inputLabel, inputParams != null))
                {
                    if (!AppendIf(inoutLabel, inoutParams != null))
                    {
                        AppendIf(outputLabel, outputParams != null);
                    }
                }
                completionItem.insertText = builder.ToString();
                completionItem.kind = proc.Profile != null && proc.Profile.IsFunction ? CompletionItemKind.Function : CompletionItemKind.Method;
                //Add specific data for eclipse completion & signatureHelper context
                completionItem.data = new object[3];
                var signatureInformation = ProcedureSignatureHelper.SignatureHelperSignatureFormatter(proc);
                ((object[])completionItem.data)[1] = signatureInformation;

                //Store the link between the hash and the procedure. This will help to determine the procedure parameter completion context later. 
                if (functionDeclarationSignatureDictionary.TryAdd(signatureInformation, proc))
                {
                    completionItems.Add(completionItem);
                }

                bool AppendIf(string str, bool condition)
                {
                    if (condition)
                    {
                        builder.Append(" ");
                        builder.Append(str);

                        return true;
                    }

                    return false;
                }
            }

            return completionItems;
        }

        public static List<CompletionItem> CreateCompletionItemsForVariableSetAndDisambiguate(IEnumerable<KeyValuePair<DataDefinitionPath, DataDefinition>> variables, TypeCobolOptions options, bool forceUnqualifiedName = false)
        {
            var results = new List<CompletionItem>();

            //Group variables by name
            var variablesGroupedByName = variables
                .GroupBy(p => p.Value.Name, StringComparer.OrdinalIgnoreCase)
                .Select(g => g.ToList());
            foreach (var variableGroup in variablesGroupedByName)
            {
                if (variableGroup.Count > 1)
                {
                    //Ambiguity: use qualified name for completion items (except if forceUnqualifiedName is set, in that case it may lead to ambiguities in completion list...)
                    foreach (var pair in variableGroup)
                    {
                        var completionItem = CreateCompletionItemForSingleVariable(pair.Key?.CurrentDataDefinition, pair.Value, options, !forceUnqualifiedName);
                        results.Add(completionItem);
                    }
                }
                else
                {
                    //Non ambiguous name, remove qualifier for pure Cobol 
                    var pair = variableGroup[0];
                    bool qualify = !options.IsCobolLanguage && !forceUnqualifiedName;
                    var completionItem = CreateCompletionItemForSingleVariable(pair.Key?.CurrentDataDefinition, pair.Value, options, qualify);
                    results.Add(completionItem);
                }
            }

            return results;
        }

        public static CompletionItem CreateCompletionItemForSingleVariable(DataDefinition parent, DataDefinition variable, TypeCobolOptions options, bool qualify, TokenType qualificationTokenType = TokenType.OF)
        {
            string name = variable.Name;
            string type = variable.DataType.Name;

            string insertText;
            if (qualify)
            {
                List<string> nameParts;
                if (parent != null)
                {
                    nameParts = parent.VisualQualifiedNameWithoutProgram.ToList();
                    nameParts.Add(name);
                }
                else
                {
                    nameParts = variable.VisualQualifiedNameWithoutProgram.ToList();
                }

                //For copies
                if (variable.CodeElement != null && variable.CodeElement.IsInsideCopy())
                {
                    nameParts.Clear();
                    string owner = parent?.VisualQualifiedNameWithoutProgram[0] ?? variable.VisualQualifiedNameWithoutProgram[0];
#if EUROINFO_RULES
                    var parts = name.Split('-');
                    if (parts.Length > 0 && !owner.Contains(parts[0]))
                        nameParts.Add(owner);
#else
                    nameParts.Add(owner);
#endif
                    if (owner != name)
                        nameParts.Add(name);
                }

                //Qualifying style depending on target language
                if (options.IsCobolLanguage)
                {
                    nameParts.Reverse();
                    var qualificationToken = TokenUtils.GetTokenStringFromTokenType(qualificationTokenType);
                    insertText = string.Join($" {qualificationToken} ", nameParts);
                }
                else
                {
                    insertText = string.Join("::", nameParts);
                }
            }
            else
            {
                insertText = name;
            }

            string label = $"{name} ({type}) ({insertText})";
            return new CompletionItem() { label = label, insertText = insertText, kind = CompletionItemKind.Variable };
        }

        private static Case GetTextCase(Token token)
        {
            string tokenText = token?.Text;

            if (string.IsNullOrEmpty(tokenText)) return Case.Lower;

            // check if upper case
            bool isUpper = true;
            foreach (char c in tokenText)
            {
                if (char.IsLower(c))
                {
                    isUpper = false;
                    break;
                }
            }

            if (isUpper) return Case.Upper;
            // check if Camel case; ex. CamlSample
            bool isCamel = false;
            int iUpperMem = -1, count = tokenText.Length;
            for (int i = 0; i < count; i++)
            {
                if (i == 0)
                {
                    // first letter
                    if (char.IsLower(tokenText[i])) break;
                    iUpperMem = 0;
                    isCamel = true;
                }
                else
                {
                    if (char.IsUpper(tokenText[i]))
                    {
                        if (iUpperMem == (i - 1))
                        {
                            // 2 upper case characters one behind the other
                            isCamel = false;
                            break;
                        }
                        iUpperMem = i;
                    }
                }
            }

            if (isCamel && iUpperMem == count - 1) isCamel = false; // last char is upper
            return (isCamel) ? Case.Camel : Case.Lower;
        }

        private static readonly Dictionary<ParameterDescription.PassingTypes, string> _UpperParams = new Dictionary<ParameterDescription.PassingTypes, string>()
        {
            { ParameterDescription.PassingTypes.Input, "INPUT"},
            { ParameterDescription.PassingTypes.Output, "OUTPUT" },
            { ParameterDescription.PassingTypes.InOut, "IN-OUT" }
        };

        private static readonly Dictionary<ParameterDescription.PassingTypes, string> _CamelParams = new Dictionary<ParameterDescription.PassingTypes, string>()
        {
            { ParameterDescription.PassingTypes.Input, "Input"},
            { ParameterDescription.PassingTypes.Output, "Output" },
            { ParameterDescription.PassingTypes.InOut, "In-Out" }
        };

        private static readonly Dictionary<ParameterDescription.PassingTypes, string> _LowerParams = new Dictionary<ParameterDescription.PassingTypes, string>()
        {
            { ParameterDescription.PassingTypes.Input, "input"},
            { ParameterDescription.PassingTypes.Output, "output" },
            { ParameterDescription.PassingTypes.InOut, "in-out" }
        };

        public static Dictionary<ParameterDescription.PassingTypes, string> GetParamsUsingMatchingCase(Token token)
        {
            switch (GetTextCase(token))
            {
                case Case.Upper:
                    return _UpperParams;
                case Case.Camel:
                    return _CamelParams;
                default:
                    return _LowerParams;
            }
        }

        private enum Case
        {
            Lower = 0,   // default value
            Upper,
            Camel
        }
    }
}

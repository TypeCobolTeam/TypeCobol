using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler;
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

        /// <summary>
        /// Get the matching node for the given CodeElement, returns null if not found.
        /// </summary>
        /// <param name="fileCompiler">Current file being compiled with its compilation results</param>
        /// <param name="codeElement">Target CodeElement</param>
        /// <returns>Corresponding Node instance, null if not found.</returns>
        public static Node GetMatchingNode(FileCompiler fileCompiler, CodeElement codeElement)
        {
            var codeElementToNode = fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot?.NodeCodeElementLinkers;
            if (codeElementToNode != null && codeElementToNode.TryGetValue(codeElement, out var node))
            {
                return node;
            }

            return null;
        }

        public static IEnumerable<string> AggregateTokens(IEnumerable<Token> tokensToAggregate)
        {
            var aggregatedTokens = new Stack<string>();

            Token previousToken = null;
            foreach (var token in tokensToAggregate)
            {
                if (previousToken != null && previousToken.TokenType == TokenType.UserDefinedWord)
                {
                    if (token.TokenType != TokenType.QualifiedNameSeparator)
                    {
                        aggregatedTokens.Push(token.Text);
                    }
                    else if (previousToken.TokenType == TokenType.UserDefinedWord)
                    {
                        var retainedString = aggregatedTokens.Pop();
                        aggregatedTokens.Push(retainedString + ".");
                    }
                }
                else if (previousToken != null && previousToken.TokenType == TokenType.QualifiedNameSeparator)
                {
                    var retainedString = aggregatedTokens.Pop();
                    aggregatedTokens.Push(retainedString + token.Text);
                }

                if (previousToken == null && token.TokenType == TokenType.UserDefinedWord)
                    aggregatedTokens.Push(token.Text);

                previousToken = token;
            }

            return aggregatedTokens.ToArray().Reverse();
        }

        public static IEnumerable<CompletionItem> CreateCompletionItemsForType(IEnumerable<TypeDefinition> types, Node node, bool enablePublicFlag = true)
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
                var completionItem = new CompletionItem(typeDisplayName);

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

        public static IEnumerable<CompletionItem> CreateCompletionItemsForProcedures(IEnumerable<FunctionDeclaration> procedures, Node node, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary,  bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            Case textCase = GetTextCase(node.CodeElement.ConsumedTokens.First(t => t.TokenType == TokenType.CALL).Text);
            Dictionary<ParameterDescription.PassingTypes, string> paramWithCase = GetParamsWithCase(textCase);

            foreach (var proc in procedures)
            {
                string inputParams = null, outputParams = null, inoutParams = null;

                if (proc.Profile != null)
                {
                    if (proc.Profile.InputParameters != null && proc.Profile.InputParameters.Count > 0)
                        inputParams = string.Format("{0} {1}",
                            paramWithCase[ParameterDescription.PassingTypes.Input],
                            string.Join(", ",
                                proc.Profile.InputParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.InoutParameters != null && proc.Profile.InoutParameters.Count > 0)
                        inoutParams = string.Format("{0} {1}",
                            paramWithCase[ParameterDescription.PassingTypes.InOut],
                            string.Join(", ",
                                proc.Profile.InoutParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.OutputParameters != null && proc.Profile.OutputParameters.Count > 0)
                        outputParams = string.Format("{0} {1}",
                            paramWithCase[ParameterDescription.PassingTypes.Output],
                            string.Join(", ",
                                proc.Profile.OutputParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));

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
                var completionItem =
                    new CompletionItem(string.Format("{0} {1} {2} {3}", procDisplayName, inputParams, inoutParams, outputParams));
                completionItem.insertText = procIsPublic
                    ? inputParams != null
                            ? string.Format("{0}::{1} {2}", proc.VisualQualifiedName.Tail, proc.VisualQualifiedName.Head, paramWithCase[ParameterDescription.PassingTypes.Input])
                            : inoutParams != null
                                ? string.Format("{0}::{1} {2}", proc.VisualQualifiedName.Tail, proc.VisualQualifiedName.Head, paramWithCase[ParameterDescription.PassingTypes.InOut])
                                : outputParams != null
                                    ? string.Format("{0}::{1} {2}", proc.VisualQualifiedName.Tail, proc.VisualQualifiedName.Head, paramWithCase[ParameterDescription.PassingTypes.Output])
                                    : string.Format("{0}::{1}", proc.VisualQualifiedName.Tail, proc.VisualQualifiedName.Head)
                    : inputParams != null
                        ? proc.Name + " " + paramWithCase[ParameterDescription.PassingTypes.Input]
                        : inoutParams != null
                            ? proc.Name + paramWithCase[ParameterDescription.PassingTypes.InOut]
                            : outputParams != null
                                ? proc.Name + " " + paramWithCase[ParameterDescription.PassingTypes.Output]
                                : proc.Name;
                completionItem.kind = proc.Profile != null && proc.Profile.IsFunction ? CompletionItemKind.Function : CompletionItemKind.Method;
                //Add specific data for eclipse completion & signatureHelper context
                completionItem.data = new object[3];
                var signatureInformation = ProcedureSignatureHelper.SignatureHelperSignatureFormatter(proc);
                ((object[])completionItem.data)[1] = signatureInformation;

                //Store the link between the hash and the procedure. This will help to determine the procedure parameter completion context later. 
                if (!functionDeclarationSignatureDictionary.ContainsKey(signatureInformation))
                {
                    functionDeclarationSignatureDictionary.Add(signatureInformation, proc);
                    completionItems.Add(completionItem);
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

        public static CompletionItem CreateCompletionItemForSingleVariable(DataDefinition parent, DataDefinition variable, TypeCobolOptions options, bool qualify)
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
                    insertText = string.Join(" OF ", nameParts);
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
            return new CompletionItem(label) { insertText = insertText, kind = CompletionItemKind.Variable };
        }

        public static Case GetTextCase(string tokenText)
        {
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

        public static Dictionary<ParameterDescription.PassingTypes, string> GetParamsWithCase(Case textCase)
        {
            switch (textCase)
            {
                case Case.Upper:
                    return _UpperParams;
                case Case.Camel:
                    return _CamelParams;
                default:
                    return _LowerParams;
            }
        }

        public enum Case
        {
            Lower = 0,   // default value
            Upper,
            Camel
        }
    }
}

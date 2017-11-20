using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    public static class CompletionFactory
    {

        #region Paragraph Completion

        /// <summary>
        /// Get the paragraph that can be associated to PERFORM Completion token.
        /// </summary>
        /// <param name="fileCompiler">The target FileCompiler instance</param>
        /// <param name="performToken">The PERFORM token</param>
        /// <returns></returns>
        public static IEnumerable<CompletionItem> GetCompletionPerformParagraph(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var performNode = GetMatchingNode(fileCompiler, codeElement);
            List<Paragraph> pargraphs = null;
            List<DataDefinition> variables = null;
            var completionItems = new List<CompletionItem>();

            if (performNode != null)
            {
                if (performNode.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    pargraphs = performNode.SymbolTable.GetParagraphs(p => p.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase));
                    variables = performNode.SymbolTable.GetVariables(da => da.Picture != null &&
                                                                           da.DataType ==
                                                                           Compiler.CodeElements.DataType.Numeric &&
                                                                           da.Name.StartsWith(userFilterText,
                                                                               StringComparison
                                                                                   .InvariantCultureIgnoreCase),
                        new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
                }
            }

            if (pargraphs != null)
            {
                completionItems.AddRange(pargraphs.Select(para => new CompletionItem(para.Name) { kind = CompletionItemKind.Reference }));
            }
            if (variables != null)
            {
                foreach (var variable in variables)
                {
                    var completionItem =
                        new CompletionItem(string.Format("{0} PIC{1}", variable.Name, variable.Picture.NormalizedValue));
                    completionItem.insertText = variable.Name;
                    completionItem.kind = CompletionItemKind.Variable;
                    completionItems.Add(completionItem);
                }
            }

            return completionItems;
        }

        #endregion

        #region Procedure Completion 
        public static IEnumerable<CompletionItem> GetCompletionForProcedure(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, codeElement);
            var procedures = new List<FunctionDeclaration>();
            var variables = new List<DataDefinition>();
            var completionItems = new List<CompletionItem>();

            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    procedures =
                        node.SymbolTable.GetFunctions(
                            f =>
                                f.QualifiedName.ToString()
                                    .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                                || f.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                            new List<SymbolTable.Scope>
                            {
                                SymbolTable.Scope.Declarations,
                                SymbolTable.Scope.Intrinsic,
                                SymbolTable.Scope.Namespace
                            });
                    variables = node.SymbolTable.GetVariables(da => da.Picture != null &&
                                                                    da.DataType ==
                                                                    Compiler.CodeElements.DataType.Alphanumeric &&
                                                                    da.Name.StartsWith(userFilterText,
                                                                        StringComparison.InvariantCultureIgnoreCase),
                        new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
                }
            }



            completionItems = CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node).ToList();

            foreach (var variable in variables)
            {
                var completionItem = new CompletionItem(string.Format("{0}", variable.Name));
                completionItem.insertText = variable.Name;
                completionItem.kind = CompletionItemKind.Variable;
                completionItems.Add(completionItem);
            }


            return completionItems;
        }
        public static IEnumerable<CompletionItem> GetCompletionForProcedureParameter(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

            //Get procedure name or qualified name
            string procedureName = string.Join(".", arrangedCodeElement.ArrangedConsumedTokens
                                            .Skip(1) //Skip the CALL token
                                            .TakeWhile(t => t.TokenType != TokenType.INPUT
                                                            && t.TokenType != TokenType.OUTPUT
                                                            && t.TokenType != TokenType.IN_OUT) // Take tokens until keyword found
                                            .Where(t => t.TokenType == TokenType.UserDefinedWord)
                                            .Select(t => t.Text));

            //Try to get procedure by its name
            var calledProcedures =
                node.SymbolTable.GetFunctions(
                    p =>
                        p.Name.Equals(procedureName) ||
                        p.QualifiedName.ToString().Equals(procedureName), new List<SymbolTable.Scope>
                    {
                        SymbolTable.Scope.Declarations,
                        SymbolTable.Scope.Intrinsic,
                        SymbolTable.Scope.Namespace
                    });


            var alreadyGivenTokens = arrangedCodeElement.ArrangedConsumedTokens
                .SkipWhile(t => t != lastSignificantToken).Skip(1)
                .TakeWhile(t => t.TokenType != TokenType.OUTPUT && t.TokenType != TokenType.IN_OUT)
                .Except(new List<Token>() { userFilterToken })
                .Where(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || t.Line <= position.line + 1);

            int alreadyGivenParametersCount = 0;
            TokenType? previousTokenType = null;

            foreach (var givenToken in alreadyGivenTokens)
            {
                if (givenToken.TokenType == TokenType.UserDefinedWord && (previousTokenType == null || previousTokenType.Value == TokenType.UserDefinedWord))
                    alreadyGivenParametersCount++;
                previousTokenType = givenToken.TokenType;
            }


            var potentialVariablesForCompletion = new List<DataDefinition>();
            foreach (var procedure in calledProcedures)
            {
                IEnumerable<ParameterDescription> procParams = null;
                //Switch to select the correct parameters profile
                #region Selective parameters Switch
                switch (lastSignificantToken.TokenType)
                {
                    case TokenType.INPUT:
                        {
                            procParams = procedure.Profile.InputParameters;
                            break;
                        }
                    case TokenType.OUTPUT:
                        {
                            procParams = procedure.Profile.OutputParameters;
                            break;
                        }
                    case TokenType.IN_OUT:
                        {
                            procParams = procedure.Profile.InoutParameters;
                            break;
                        }
                    default:
                        procParams = new List<ParameterDescription>();
                        break;
                }

                #endregion

                //If the user already written all or more parameters than required let's check for an other proc signature
                if (alreadyGivenParametersCount >= procParams.Count())
                    continue;

                //Else see which parameter could be filled
                var parameterToFill = procParams.ToArray()[alreadyGivenParametersCount];
                //Get local/global variable that could correspond to the parameter

                node.SymbolTable.GetVariablesByType(parameterToFill.DataType, ref potentialVariablesForCompletion, new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });

            }

            foreach (var potentialVariable in potentialVariablesForCompletion.Where(v => v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)).Distinct())
                SearchVariableInTypesAndLevels(node, potentialVariable, ref completionItems); //Add potential variables to completionItems

            return completionItems;
        }

        #endregion

        #region Library Completion
        public static IEnumerable<CompletionItem> GetCompletionForLibrary(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var callNode = GetMatchingNode(fileCompiler, codeElement);
            List<Program> programs = new List<Program>();
            if (callNode != null)
            {
                if (callNode.SymbolTable != null)
                {
                    programs =
                        callNode.SymbolTable.GetPrograms(userFilterToken != null ? userFilterToken.Text : string.Empty);
                }
            }

            var completionItems = new List<CompletionItem>();
            foreach (var prog in programs)
            {
                var completionItem = new CompletionItem(prog.Name);
                completionItem.kind = CompletionItemKind.Module;
                completionItems.Add(completionItem);
            }


            return completionItems;
        }

        #endregion

        #region Types Completion
        public static IEnumerable<CompletionItem> GetCompletionForType(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, codeElement);
            var types = new List<TypeDefinition>();
            var completionItems = new List<CompletionItem>();

            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    types =
                        node.SymbolTable.GetTypes(
                            t => t.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                                 ||
                                 t.QualifiedName.ToString()
                                     .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                            new List<SymbolTable.Scope>
                            {
                                SymbolTable.Scope.Declarations,
                                SymbolTable.Scope.Global,
                                SymbolTable.Scope.Intrinsic,
                                SymbolTable.Scope.Namespace
                            });
                }
            }

            return CompletionFactoryHelpers.CreateCompletionItemsForType(types, node);
        }
        #endregion

        #region QualifiedName Completion
        public static IEnumerable<CompletionItem> GetCompletionForQualifiedName(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token qualifiedNameSeparatorToken, Token userFilterToken)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

            //Get the token before MatchingToken 
            var userTokenToSeek =
                arrangedCodeElement.ArrangedConsumedTokens.ElementAt(
                    arrangedCodeElement.ArrangedConsumedTokens.IndexOf(qualifiedNameSeparatorToken) - 1);
            var qualifiedNameTokens = new List<Token>();
            qualifiedNameTokens.AddRange(
                    arrangedCodeElement.ArrangedConsumedTokens.Where(
                        t => t.TokenType == TokenType.UserDefinedWord || t.TokenType == TokenType.QualifiedNameSeparator));
            //Remove all the userdefinedword token and also QualifiedNameToken
            arrangedCodeElement.ArrangedConsumedTokens = arrangedCodeElement.ArrangedConsumedTokens.Except(qualifiedNameTokens).ToList();
            //We only wants the token that in front of any QualifiedName 
            //Get the first significant token (i.e CALL/TYPE/...)
            Token firstSignificantToken, tempUserFilterToken;
            CodeElementMatcher.MatchCompletionCodeElement(position, new List<CodeElementWrapper> { arrangedCodeElement }, out tempUserFilterToken,
                out firstSignificantToken);

            //For MOVE INPUT OUTPUT variables etc.. , get all the childrens of a variable that are accessible
            //Try to find corresponding variables
            string qualifiedName = string.Join(".",
                        qualifiedNameTokens.Where(
                                t =>
                                    t.TokenType == TokenType.UserDefinedWord &&
                                    !(t.Text == userFilterText && userFilterToken != null && t.StartIndex == userFilterToken.StartIndex && t.EndColumn == userFilterToken.EndColumn) &&
                                    ((t.StartIndex >= firstSignificantToken.EndColumn &&
                                      t.Line == firstSignificantToken.Line) || t.Line > firstSignificantToken.Line) &&
                                      ((t.EndColumn <= position.character && t.Line == position.line + 1) || t.Line < position.line + 1))
                            .Select(t => t.Text));
            var possibleVariables = node.SymbolTable.GetVariablesExplicit(new URI(qualifiedName));

            if (possibleVariables != null && possibleVariables.Count > 0)
            {
                //Get childrens of a type to get completion possibilities
                foreach (var variable in possibleVariables)
                {
                    var childrens = new List<Node>();
                    if (variable.Children != null && variable.Children.Count > 0) //It's a variable with levels inside
                        childrens.AddRange(variable.Children);
                    else //It's a typed variable, we have to search for childrens in the type
                    {
                        var typeChildrens = GetTypeChildrens(node.SymbolTable, variable);
                        if (typeChildrens != null)
                            childrens.AddRange(typeChildrens.Where(t => t.Name != null));
                    }

                    completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(
                        childrens.Where(
                                c => c.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)) //Filter on user text
                            .Select(child => child as DataDefinition), false));
                }
            }
            else
            { //If no variables found, it's could be a children declared in a typedef..
                var childrens = new List<Node>();
                var potentialTypes =
                    node.SymbolTable.GetTypes(
                        t =>
                            t.Children != null &&
                            t.Children.Any(
                                tc => tc.Name != null && tc.Name.Equals(userTokenToSeek.Text, StringComparison.InvariantCultureIgnoreCase)),
                        new List<SymbolTable.Scope>
                        {
                            SymbolTable.Scope.Declarations,
                            SymbolTable.Scope.Global,
                            SymbolTable.Scope.Intrinsic,
                            SymbolTable.Scope.Namespace
                        });

                foreach (var nodeType in potentialTypes.SelectMany(t => t.Children).Where(c => c != null && c.Name != null && c.Name.Equals(userTokenToSeek.Text, StringComparison.InvariantCultureIgnoreCase)))
                {

                    var nodeDataDef = nodeType as DataDefinition;
                    if (nodeDataDef == null) continue;

                    var typeChildrens = GetTypeChildrens(node.SymbolTable, nodeDataDef);
                    if (typeChildrens != null)
                        childrens.AddRange(typeChildrens);
                }

                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(
                        childrens.Where(
                                c => c.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)) //Filter on user text
                            .Select(child => child as DataDefinition), false));
            }

            if (firstSignificantToken != null)
            {
                switch (firstSignificantToken.TokenType)
                {
                    case TokenType.CALL:
                        {
                            //On CALL get possible procedures and functions in the seeked program
                            var programs = node.SymbolTable.GetPrograms(userTokenToSeek.Text);
                            if (programs != null && programs.Count > 0)
                            {
                                var procedures =
                                    programs.First()
                                        .SymbolTable.GetFunctions(
                                            f =>
                                                f.Name.StartsWith(userFilterText,
                                                    StringComparison.InvariantCultureIgnoreCase) ||
                                                f.QualifiedName.ToString()
                                                    .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                            new List<SymbolTable.Scope>
                                            {
                                            SymbolTable.Scope.Declarations
                                            });
                                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, false));

                            }
                            break;
                        }
                    case TokenType.TYPE:
                        {
                            //On TYPE get possible public types in the seeked program
                            var programs = node.SymbolTable.GetPrograms(userTokenToSeek.Text);
                            if (programs != null && programs.Count > 0)
                            {
                                var types =
                                    programs.First()
                                        .SymbolTable.GetTypes(
                                            t =>
                                                t.Name.StartsWith(userFilterText,
                                                    StringComparison.InvariantCultureIgnoreCase),
                                            new List<SymbolTable.Scope>
                                            {
                                            SymbolTable.Scope.Declarations,
                                            SymbolTable.Scope.Global
                                            });
                                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForType(types, node, false));
                            }
                            break;
                        }
                }
            }

            return completionItems.Distinct();
        }
        #endregion

        #region Variable Completion
        public static IEnumerable<CompletionItem> GetCompletionForVariable(FileCompiler fileCompiler, CodeElement codeElement, Expression<Func<DataDefinition, bool>> predicate)
        {
            var completionItems = new List<CompletionItem>();
            var node = GetMatchingNode(fileCompiler, codeElement);
            List<DataDefinition> variables = null;

            variables = node.SymbolTable.GetVariables(predicate, new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(variables));

            return completionItems;
        }
        #endregion

        #region TO Completion
        public static IEnumerable<CompletionItem> GetCompletionForTo(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            List<DataDefinition> potentialVariables = new List<DataDefinition>();
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

            var qualifiedNameTokens = arrangedCodeElement.ArrangedConsumedTokens.SkipWhile(t => t.TokenType != TokenType.UserDefinedWord).TakeWhile(t => t != lastSignificantToken).Where(t => t.TokenType != TokenType.QualifiedNameSeparator);
            if (!qualifiedNameTokens.Any())
                return completionItems;


            DataType seekedDataType = null;
            var foundedVar = node.SymbolTable.GetVariablesExplicit(new URI(string.Join(".", qualifiedNameTokens.Select(t => t.Text))));


            if (foundedVar.Count != 1) //If no varaible or more than one founded stop process
                return completionItems;

            seekedDataType = foundedVar.First().DataType;

            node.SymbolTable.GetVariablesByType(seekedDataType, ref potentialVariables, new List<SymbolTable.Scope> { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global });
            potentialVariables = potentialVariables.Where(da => (da.CodeElement != null && ((DataDefinitionEntry) da.CodeElement).LevelNumber.Value < 88) || (da.CodeElement == null && da is IndexDefinition)).ToList(); //Ignore variable of level 88. 

            foreach (var potentialVariable in potentialVariables) //Those variables could be inside a typedef or a level, we need to check to rebuild the qualified name correctly.
            {
                SearchVariableInTypesAndLevels(node, potentialVariable, ref completionItems);
            }

            completionItems.Remove(
                completionItems.FirstOrDefault(
                    c => c.label.Contains(string.Join("::", qualifiedNameTokens.Select(t => t.Text)))));

            return completionItems.Where(c => c.insertText.IndexOf(userFilterText, StringComparison.InvariantCultureIgnoreCase) >= 0);
        }
        #endregion

        #region OF Completion
        /// <summary>
        /// Get completion after OF Cobol Keyword. This method also adjust take in account the token before OF.
        /// </summary>
        /// <param name="fileCompiler"></param>
        /// <param name="codeElement"></param>
        /// <param name="userFilterToken"></param>
        /// <returns></returns>
        public static IEnumerable<CompletionItem> GetCompletionForOf(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Position position)
        {
            var completionItems = new List<CompletionItem>();
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);

            var tokensUntilCursor = arrangedCodeElement.ArrangedConsumedTokens
            .Except(new List<Token>() { userFilterToken })
            .Where(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || t.Line < position.line + 1).Reverse();
            
            //Detect what's before the OF token
            var tokenBeforeOf = tokensUntilCursor.Skip(1).FirstOrDefault(); //Skip(1) will skip the OF token

            if (tokenBeforeOf == null || tokenBeforeOf.TokenType != TokenType.ADDRESS) //For now we only need to filter on adress. 
                return null;

            switch (tokenBeforeOf.TokenType) //In the future, this will allow to switch between different token declared before OF. 
            {
                case TokenType.ADDRESS:
                {
                    var contextToken = tokensUntilCursor.Skip(2).FirstOrDefault(); //Try to get the token that may define the completion context
                    GetCompletionForAddressOf(ref completionItems, node, contextToken, userFilterText);
                    break;
                }  
            }

            return completionItems;
        }

        /// <summary>
        /// Get completion Item for a types ADDRESS OF demand. 
        /// This meythod will modify the completionItems ref parameter.
        /// CompletionItems will be filtered on varaibles declared in LINKAGE SECTION and with LevelNumber equal to 01 or 77. 
        /// </summary>
        /// <param name="completionItems"></param>
        /// <param name="aarangedCodeElement"></param>
        /// <param name="matchingNode"></param>
        /// <param name="userFilterText"></param>
        public static void GetCompletionForAddressOf(ref List<CompletionItem> completionItems, Node node, Token contextToken, string userFilterText)
        {
            var potentialVariable = new List<DataDefinition>();

            if (contextToken != null && contextToken.TokenType == TokenType.SET)
            {
                //Get all the varaibles in Linkage section with Level 01 or 77 and starting by userFilterText. 
                potentialVariable.AddRange(node.SymbolTable.GetVariables(v => v.IsFlagSet(Node.Flag.LinkageSectionNode)
                                                && ((v.CodeElement as DataDefinitionEntry).LevelNumber.Value == 1 || (v.CodeElement as DataDefinitionEntry).LevelNumber.Value == 77)
                                                && v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                new List<SymbolTable.Scope>() { SymbolTable.Scope.Declarations, SymbolTable.Scope.Global }));
            }
            else 
            {
                //Get all the variables from any section with level 01 or 77 and starting by userFilterText.
                potentialVariable.AddRange(node.SymbolTable.GetVariables(
                        v => ((v.CodeElement as DataDefinitionEntry).LevelNumber.Value == 1 ||
                             (v.CodeElement as DataDefinitionEntry).LevelNumber.Value == 77) &&
                             v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                        new List<SymbolTable.Scope>() {SymbolTable.Scope.Declarations, SymbolTable.Scope.Global}));
            }
           

            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(potentialVariable));
        }
        #endregion



        #region Helpers

        public static IReadOnlyList<Node> GetTypeChildrens(SymbolTable symbolTable, DataDefinition dataDefNode)
        {
            if (symbolTable == null || dataDefNode == null)
                return null;

            var type = symbolTable.GetTypes(
                t => t.Name.Equals(dataDefNode.DataType.Name, StringComparison.InvariantCultureIgnoreCase)
                     ||
                     t.QualifiedName.ToString()
                         .Equals(dataDefNode.DataType.Name, StringComparison.InvariantCultureIgnoreCase),
                new List<SymbolTable.Scope>
                {
                    SymbolTable.Scope.Declarations,
                    SymbolTable.Scope.Global,
                    SymbolTable.Scope.Intrinsic,
                    SymbolTable.Scope.Namespace
                }).FirstOrDefault();

            if (type != null)
                return type.Children;
            else
                return null;
        }

        /// <summary>
        /// Get the matchig node for a given Token and a gien completion mode. Returning a matching Node or null.
        /// </summary>
        /// <param name="fileCompiler"></param>
        /// <param name="codeElement"></param>
        /// <returns></returns>
        public static Node GetMatchingNode(FileCompiler fileCompiler, CodeElement codeElement)
        {
            if (fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot != null
                && fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers != null)
            {
                return
                    fileCompiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.NodeCodeElementLinkers
                        .FirstOrDefault(t => t.Key.Equals(codeElement)).Value;
            }

            return null;
        }

        private static void SearchVariableInTypesAndLevels(Node node, DataDefinition variable, ref List<CompletionItem> completionItems)
        {
            var symbolTable = node.SymbolTable;
            if (variable.GetParentTypeDefinition == null)  //Variable is not comming from a type. 
            {
                if (symbolTable.GetVariablesExplicit(new URI(variable.Name)).Count > 0)   //Check if this variable is present locally. 
                {
                    completionItems.Add(CompletionFactoryHelpers.CreateCompletionItemForVariable(variable));
                }
            }
            else
            {
                if (symbolTable.TypesReferences != null) //We are in a typedef, get references of this type
                {
                    var types = symbolTable.GetType(variable.GetParentTypeDefinition.DataType);
                    IEnumerable<DataDefinition> references = null;
                    if (types.Count == 0)
                    {
                        references = symbolTable.TypesReferences.SelectMany(t => t.Value);
                    }
                    else
                    {
                        var type = types.First();
                        references = symbolTable.TypesReferences.Where(t => t.Key == type).SelectMany(r => r.Value);
                    }

                    foreach (var reference in references)
                    {
                        if (symbolTable.GetVariablesExplicit(new URI(reference.Name)).Count > 0)  //Check if this variable is present locally. If not just ignore it
                        {
                            if (reference.GetParentTypeDefinition == null) //Check if the variable is inside a typedef or not, if not it's a final varaible
                            {
                                var referenceArrangedQualifiedName = string.Join("::", reference.QualifiedName.ToString().Split(reference.QualifiedName.Separator).Skip(1)); //Skip Program Name
                                var finalQualifiedName = string.Format("{0}::{1}", referenceArrangedQualifiedName, variable.QualifiedName.Head);
                                var variableDisplay = string.Format("{0} ({1}) ({2})", variable.Name, variable.DataType.Name, finalQualifiedName);
                                completionItems.Add(new CompletionItem(variableDisplay) { insertText = finalQualifiedName, kind = CompletionItemKind.Variable });
                            }
                            else //If the reference is always in a typedef, let's loop and ride up until we are in a final variable
                            {
                                var tempCompletionItems = new List<CompletionItem>();
                                SearchVariableInTypesAndLevels(node, reference, ref tempCompletionItems);

                                if (tempCompletionItems.Count > 0)
                                {
                                    foreach (var tempComp in tempCompletionItems)
                                    {
                                        tempComp.insertText += "::" + variable.QualifiedName.Head;
                                        tempComp.label = string.Format("{0} ({1}) ({2})", variable.Name, variable.DataType.Name, tempComp.insertText);
                                        completionItems.Add(tempComp);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        #endregion
    }
}

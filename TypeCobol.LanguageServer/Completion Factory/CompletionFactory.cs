using System;
using System.Collections;
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
            IEnumerable<Paragraph> pargraphs = null;
            IEnumerable<DataDefinition> variables = null;
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
                                                                           da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                                           SymbolTable.Scope.GlobalStorage);
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
        public static IEnumerable<CompletionItem> GetCompletionForProcedure(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary)
        {
            IEnumerable<FunctionDeclaration> procedures = null;
            IEnumerable<DataDefinition> variables = null;
            var completionItems = new List<CompletionItem>();
            var node = GetMatchingNode(fileCompiler, codeElement);
            if(node == null)
                return completionItems;

            if (node != null)
            {
                if (node.SymbolTable != null)
                {
                    var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                    procedures =
                        node.SymbolTable.GetFunctions(
                            f =>
                                f.VisualQualifiedName.ToString()
                                    .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                                || f.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                SymbolTable.Scope.Intrinsic
);
                    variables = node.SymbolTable.GetVariables(da => da.Picture != null &&
                                                                    da.DataType ==
                                                                    Compiler.CodeElements.DataType.Alphanumeric &&
                                                                    da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase), 
                                                                    SymbolTable.Scope.GlobalStorage );
                }
            }

            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, functionDeclarationSignatureDictionary));

            foreach (var variable in variables)
            {
                var completionItem = new CompletionItem(string.Format("{0}", variable.Name));
                completionItem.insertText = variable.Name;
                completionItem.kind = CompletionItemKind.Variable;
                completionItems.Add(completionItem);
            }


            return completionItems;
        }
        public static IEnumerable<CompletionItem> GetCompletionForProcedureParameter(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken, FunctionDeclaration procedureSignatureContext)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return completionItems;

            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

            //Get procedure name or qualified name
            string procedureName = CompletionFactoryHelpers.GetProcedureNameFromTokens(arrangedCodeElement?.ArrangedConsumedTokens);
            IEnumerable<FunctionDeclaration> calledProcedures = null;

            if (procedureSignatureContext == null ||
                !(procedureSignatureContext.QualifiedName.ToString().Equals(procedureName, StringComparison.InvariantCultureIgnoreCase) 
                  || procedureSignatureContext.Name.Equals(procedureName, StringComparison.InvariantCultureIgnoreCase)))
            {
                //Try to get procedure by its name
                calledProcedures =
                    node.SymbolTable.GetFunctions(
                        p =>
                            p.Name.Equals(procedureName, StringComparison.InvariantCultureIgnoreCase) ||
                            p.VisualQualifiedName.ToString().Equals(procedureName, StringComparison.InvariantCultureIgnoreCase),
                            SymbolTable.Scope.Intrinsic
                        );
            }
            else
            {
                //If the procedure name is equivalent to the signature selected by signature help, we can assume the user is always on the same procedure. 
                calledProcedures = new List<FunctionDeclaration> {procedureSignatureContext};
            }

            var alreadyGivenTokens = arrangedCodeElement?.ArrangedConsumedTokens
                .SkipWhile(t => t != lastSignificantToken).Skip(1)
                .TakeWhile(t => t.TokenType != TokenType.OUTPUT && t.TokenType != TokenType.IN_OUT)
                .Except(new List<Token>() { userFilterToken })
                .Where(t => (t.StartIndex < position.character && t.Line == position.line + 1) || t.Line < position.line + 1);

            int alreadyGivenParametersCount = 0;
            TokenType? previousTokenType = null;

            //Loop that allows to ignore qualified name parameters. 
            if (alreadyGivenTokens != null)
                foreach (var givenToken in alreadyGivenTokens)
                {
                    if (givenToken.TokenType == TokenType.UserDefinedWord && (previousTokenType == null || previousTokenType.Value == TokenType.UserDefinedWord))
                        alreadyGivenParametersCount++;
                    previousTokenType = givenToken.TokenType;
                }

            
            IEnumerable<DataDefinition> potentialVariablesForCompletion = null;
            int maxInput = -1;

            int maxInOut = -1;
            bool allProcsHaveInOutParams = true;
            bool noProcsHaveInOutParams = true;
            
            bool allProcsHaveOutputParams = true;

            foreach (var procedure in calledProcedures)
            {
                IEnumerable<ParameterDescription> procParams;

                //Switch to select the correct parameters profile
                #region Selective parameters Switch
                switch (lastSignificantToken.TokenType)
                {
                    case TokenType.INPUT:
                        {
                            procParams = procedure.Profile.InputParameters;
                            //Get number of input parameters left per procedure
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
                            //Get number of inout parameters left per procedure
                            break;
                        }
                    default:
                        procParams = new List<ParameterDescription>();
                        break;
                }
                #endregion

                if (procedure.Profile.InputParameters.Count > maxInput) {
                    maxInput = procedure.Profile.InputParameters.Count;
                }
                if (procedure.Profile.InoutParameters.Count > maxInOut) {
                    maxInOut = procedure.Profile.InoutParameters.Count;
                }

                if (procedure.Profile.InoutParameters.Count == 0) {
                    allProcsHaveInOutParams = false;
                } else {
                    noProcsHaveInOutParams = false;
                }
                if (procedure.Profile.OutputParameters.Count == 0) {
                    allProcsHaveOutputParams = false;
                }
                
                
                //If the user already written all or more parameters than required let's check for an other proc signature
                if (alreadyGivenParametersCount >= procParams.Count())
                    continue;

                //Else see which parameter could be filled
                var parameterToFill = procParams.ToArray()[alreadyGivenParametersCount];
                //Get local/global variable that could correspond to the parameter

                potentialVariablesForCompletion = node.SymbolTable.GetVariablesByType(parameterToFill.DataType, potentialVariablesForCompletion, SymbolTable.Scope.GlobalStorage);

            }

            if (potentialVariablesForCompletion == null) return completionItems;

            foreach (var potentialVariable in potentialVariablesForCompletion.Where(v => v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)).Distinct())
                SearchVariableInTypesAndLevels(node, potentialVariable, ref completionItems); //Add potential variables to completionItems*           


            //If signature of procedure is available
            if (procedureSignatureContext != null)
            {
                //Add IN-OUT or OUTPUT after INPUT ?
                if (lastSignificantToken.TokenType == TokenType.INPUT
                    && alreadyGivenParametersCount == (procedureSignatureContext.Profile.InputParameters.Count - 1))
                {
                    if (procedureSignatureContext.Profile.InoutParameters.Count != 0) {
                        AddIn_OutSuffixToCompletionItems(lastSignificantToken, completionItems);

                    }
                    else if (procedureSignatureContext.Profile.OutputParameters.Count != 0)
                    {
                        AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems);
                    }
                }

                //Add OUTPUT after IN-OUT ?
                else if (lastSignificantToken.TokenType == TokenType.IN_OUT 
                         && alreadyGivenParametersCount == (procedureSignatureContext.Profile.InoutParameters.Count - 1)
                         && procedureSignatureContext.Profile.OutputParameters.Count != 0) {
                    AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems);
                }
            }
            else
            {
                //Add IN-OUT or OUTPUT after INPUT ?
                //If we reach the last INPUT parameter
                if (lastSignificantToken.TokenType == TokenType.INPUT && alreadyGivenParametersCount == maxInput -1) {


                    //If all procs have IN-OUT params
                    if (allProcsHaveInOutParams) {
                        AddIn_OutSuffixToCompletionItems(lastSignificantToken, completionItems);
                    }
                    //If no procedures have IN-OUT params and all have OUTPUT params
                    else if (noProcsHaveInOutParams && allProcsHaveOutputParams)
                    {
                        AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems);
                    }
                    //Otherwise we cannot choose between IN-OUT, OUTPUT and nothing, so we choose nothing and let the user add the good keyword manually.
                    //#908 will change this behavior by asking for the signature context
                }

                //Add OUTPUT after IN-OUT ?
                else if (lastSignificantToken.TokenType == TokenType.IN_OUT && alreadyGivenParametersCount == (maxInOut - 1))
                {
                    //If all procedures have OUTPUT parameter
                    if (allProcsHaveOutputParams)
                    {
                        AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems);
                    }
                }
            }

            return completionItems;
        }

        private static void AddIn_OutSuffixToCompletionItems(Token lastSignificantToken, List<CompletionItem> completionItems) {
            //Use -1, because it seems LSP start counting at 1
            var suffix = "\n" + new string(' ', lastSignificantToken.Column-1) + "IN-OUT ";
            completionItems.ForEach(ci => ci.insertText += suffix);
        }
        private static void AddOutputSuffixToCompletionItems(Token lastSignificantToken, List<CompletionItem> completionItems) {
            //Use -1, because it seems LSP start counting at 1
            var suffix = "\n" + new string(' ', lastSignificantToken.Column-1) + "OUTPUT ";
            completionItems.ForEach(ci => ci.insertText += suffix);
        }
    

        #endregion

        #region Library Completion
        public static IEnumerable<CompletionItem> GetCompletionForLibrary(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var callNode = GetMatchingNode(fileCompiler, codeElement);
            IEnumerable<Program> programs = null;
            if (callNode?.SymbolTable != null)
            {
                programs =
                    callNode.SymbolTable.GetPrograms(userFilterToken != null ? userFilterToken.Text : string.Empty);
            }

            return programs?.Select(prog => new CompletionItem(prog.Name) {kind = CompletionItemKind.Module}) ?? new List<CompletionItem>();
        }

        #endregion

        #region Types Completion
        public static IEnumerable<CompletionItem> GetCompletionForType(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = GetMatchingNode(fileCompiler, codeElement);
            IEnumerable<TypeDefinition> types = null;
            if (node?.SymbolTable == null)
                return new List<CompletionItem>();

            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
            types =
                node.SymbolTable.GetTypes(
                    t => t.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)
                         ||
                         (!t.IsFlagSet(Node.Flag.NodeIsIntrinsic) &&
                          t.VisualQualifiedName.ToString()
                              .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)),
                          SymbolTable.Scope.Intrinsic
                    );
            

            return CompletionFactoryHelpers.CreateCompletionItemsForType(types, node);
        }
        #endregion

        #region QualifiedName Completion
        public static IEnumerable<CompletionItem> GetCompletionForQualifiedName(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token qualifiedNameSeparatorToken, Token userFilterToken, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return completionItems;
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;

            //Get the token before MatchingToken 
            var userTokenToSeek =
                arrangedCodeElement?.ArrangedConsumedTokens.ElementAt(
                    arrangedCodeElement.ArrangedConsumedTokens.IndexOf(qualifiedNameSeparatorToken) - 1);
            var qualifiedNameTokens = new List<Token>();
            if (arrangedCodeElement != null)
            {
                qualifiedNameTokens.AddRange(
                    arrangedCodeElement.ArrangedConsumedTokens?.Where(
                        t => (t?.TokenType == TokenType.UserDefinedWord || t?.TokenType == TokenType.QualifiedNameSeparator) && (t.EndColumn <= position.character && t.Line == position.line + 1) || t.Line < position.line + 1));
                //Remove all the userdefinedword token and also QualifiedNameToken
                arrangedCodeElement.ArrangedConsumedTokens = arrangedCodeElement.ArrangedConsumedTokens.Except(qualifiedNameTokens).ToList();
                //We only wants the token that in front of any QualifiedName 
                //Get the first significant token (i.e CALL/TYPE/...)
                Token firstSignificantToken, tempUserFilterToken;
                CodeElementMatcher.MatchCompletionCodeElement(position, new List<CodeElementWrapper> { arrangedCodeElement }, out tempUserFilterToken,
                    out firstSignificantToken);


                //Select the qualifiedName chain closest to cursor
                Token previousToken = null;
                qualifiedNameTokens.Reverse();
                var filteredQualifiedNameTokens = new List<Token>(); //Will contains all the tokens forming the qualifiedName chain. 
                foreach (var token in qualifiedNameTokens)
                {
                    if (previousToken == null 
                        || ((previousToken.TokenType == TokenType.QualifiedNameSeparator && token.TokenType == TokenType.UserDefinedWord) 
                            || (token.TokenType == TokenType.QualifiedNameSeparator && previousToken.TokenType == TokenType.UserDefinedWord)))
                        filteredQualifiedNameTokens.Add(token);
                    else
                        break;

                    previousToken = token;
                }
                filteredQualifiedNameTokens.Reverse();

               //For MOVE INPUT OUTPUT variables etc.. , get all the children of a variable that are accessible
               //Try to find corresponding variables
               var qualifiedName = 
                    filteredQualifiedNameTokens.Where(
                            t =>
                                t.TokenType == TokenType.UserDefinedWord &&
                                !(t.Text == userFilterText && userFilterToken != null &&
                                  t.StartIndex == userFilterToken.StartIndex && t.EndColumn == userFilterToken.EndColumn) &&
                                ((firstSignificantToken != null && ((t.StartIndex >= firstSignificantToken.EndColumn && t.Line == firstSignificantToken.Line) || t.Line > firstSignificantToken.Line)) 
                                || firstSignificantToken == null) 
                                && ((t.EndColumn <= position.character && t.Line == position.line + 1) || t.Line < position.line + 1))
                        .Select(t => t.Text);
                qualifiedName.Reverse();
                var possibleVariables = node.SymbolTable.GetVariablesExplicit(new URI(qualifiedName));

                if (possibleVariables != null && possibleVariables.Any()) 
                {
                    //Get children of a type to get completion possibilities
                    foreach (var variable in possibleVariables)
                    {
                        var children = new List<Node>();
                        if (variable.Children != null && variable.Children.Count > 0) //It's a variable with levels inside
                            children.AddRange(variable.Children);
                        else //It's a typed variable, we have to search for children in the type
                        {
                            var typeChildren = GetTypeChildren(node.SymbolTable, variable);
                            if (typeChildren != null)
                                children.AddRange(typeChildren.Where(t => t.Name != null || t.Children.Where(u => u.Name != null) != null));
                        }

                        var computedChildrenList = new List<Node>();
                        foreach (var child in children)
                        {
                            GetNextRelevantChildren(child, computedChildrenList);
                        }

                        completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(
                            computedChildrenList.Where(
                                    c => c.Name != null && c.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)) //Filter on user text
                                .Select(child => child as DataDefinition), false));
                    }
                }
                else
                { //If no variables found, it's could be a children declared in a typedef..
                    var children = new List<Node>();
                    var potentialTypes =
                        node.SymbolTable.GetTypes(
                            t =>
                                t.Children != null &&
                                t.Children.Any(
                                    tc => tc.Name != null && tc.Name.Equals(userTokenToSeek.Text, StringComparison.InvariantCultureIgnoreCase)),
                                SymbolTable.Scope.Namespace
                            );

                    foreach (var nodeType in potentialTypes.SelectMany(t => t.Children).Where(c => c != null && c.Name != null && c.Name.Equals(userTokenToSeek.Text, StringComparison.InvariantCultureIgnoreCase)))
                    {

                        var nodeDataDef = nodeType as DataDefinition;
                        if (nodeDataDef == null) continue;

                        var typeChildrens = GetTypeChildren(node.SymbolTable, nodeDataDef);
                        if (typeChildrens != null)
                            children.AddRange(typeChildrens);
                    }

                    completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(
                        children.Where(
                                c => c.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)) //Filter on user text
                            .Select(child => child as DataDefinition), false));
                }

                if (firstSignificantToken != null)
                {
                    switch (firstSignificantToken.TokenType)
                    {
                        case TokenType.CALL:
                        {
                            functionDeclarationSignatureDictionary.Clear(); //Clear to avoid key collision
                            //On CALL get possible procedures and functions in the seeked program
                            var programs = node.SymbolTable.GetPrograms(userTokenToSeek.Text);
                            if (programs != null && programs.Any())
                            {
                                var procedures =
                                    programs.First()
                                        .SymbolTable.GetFunctions(
                                            f =>
                                                f.Name.StartsWith(userFilterText,
                                                    StringComparison.InvariantCultureIgnoreCase) ||
                                                f.VisualQualifiedName.ToString()
                                                    .StartsWith(userFilterText,
                                                        StringComparison.InvariantCultureIgnoreCase),
                                            SymbolTable.Scope.Declarations);

                                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, functionDeclarationSignatureDictionary, false));

                            }
                            break;
                        }
                        case TokenType.TYPE:
                        {
                            //On TYPE get possible public types in the seeked program
                            var programs = node.SymbolTable.GetPrograms(userTokenToSeek.Text);
                            if (programs != null && programs.Any())
                            {
                                var types =
                                    programs.First()
                                        .SymbolTable.GetTypes(
                                            t =>
                                                t.Name.StartsWith(userFilterText,
                                                    StringComparison.InvariantCultureIgnoreCase),
                                                SymbolTable.Scope.GlobalStorage
                                            );
                                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForType(types, node, false));
                            }
                            break;
                        }
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
            if (node == null)
                return completionItems;

            var variables = node.SymbolTable.GetVariables(predicate, SymbolTable.Scope.GlobalStorage);
            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariables(variables));

            return completionItems;
        }
        #endregion

        #region TO Completion
        public static IEnumerable<CompletionItem> GetCompletionForTo(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken)
        {
            List<DataType> seekedDataTypes = new List<DataType>();
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            if (arrangedCodeElement == null)
                return completionItems;
            var node = GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return completionItems;

            IEnumerable<DataDefinition> potentialVariables = null;
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
            Expression<Func<DataDefinition, bool>> variablePredicate =
                da =>
                    (da.CodeElement != null && (da.CodeElement).LevelNumber.Value < 88) ||
                    (da.CodeElement == null && da is IndexDefinition); //Ignore variable of level 88.

            //Look if the sending variable is like litteral Alpha / Numeric

            if (node.CodeElement is MoveSimpleStatement)
            {
                var sendingItem = ((MoveSimpleStatement)node.CodeElement).SendingItem;
                var sendingVar = ((MoveSimpleStatement)node.CodeElement).SendingVariable;
                if (sendingItem != null)
                {
                    if (!(sendingItem is QualifiedName))
                    {
                        if (sendingItem is bool?) seekedDataTypes.Add(DataType.Boolean);
                        if (sendingItem is double?) seekedDataTypes.Add(DataType.Numeric);
                        if (sendingItem is string) seekedDataTypes.Add(DataType.Alphanumeric);
                    }

                    if (sendingVar != null && sendingVar.IsLiteral == true && sendingItem is double? &&
                        (sendingVar.NumericValue.Token.TokenType == TokenType.ZERO
                         || sendingVar.NumericValue.Token.TokenType == TokenType.ZEROS
                         || sendingVar.NumericValue.Token.TokenType == TokenType.ZEROES))
                    {
                        //See #845, ZERO support Alphabetic/Numeric/Alphanumeric datatype
                        seekedDataTypes.Add(DataType.Alphanumeric);
                        seekedDataTypes.Add(DataType.Alphabetic); 
                    }
                }

                if (sendingVar?.RepeatedCharacterValue?.Token != null &&
                    (sendingVar.RepeatedCharacterValue.Token.TokenType == TokenType.SPACE
                     || sendingVar.RepeatedCharacterValue.Token.TokenType == TokenType.SPACES))
                {
                    seekedDataTypes.Add(DataType.Alphabetic);
                }
            }


            bool unsafeContext = arrangedCodeElement.ArrangedConsumedTokens.Any(t => t != null && t.TokenType == TokenType.UNSAFE);
            var filteredTokens = arrangedCodeElement.ArrangedConsumedTokens.SkipWhile(t => t.TokenType != TokenType.UserDefinedWord);
            bool needTailReverse = filteredTokens.Any(t => t.TokenType == TokenType.OF);
            var qualifiedNameTokens = filteredTokens.TakeWhile(t => t != lastSignificantToken).Where(t => !(t.TokenType == TokenType.QualifiedNameSeparator || t.TokenType == TokenType.OF) && t != userFilterToken);
            if (!qualifiedNameTokens.Any() && seekedDataTypes.Count == 0)
                return completionItems;

            if (needTailReverse)
                qualifiedNameTokens = qualifiedNameTokens.Reverse();

            if (!unsafeContext) //Search for variable that match the DataType
            {
                if (seekedDataTypes.Count == 0) //If a Datatype hasn't be found yet. 
                {
                    var foundedVars =
                        node.SymbolTable.GetVariablesExplicit(
                            new URI(qualifiedNameTokens.Select(t => t.Text)));

                    if (foundedVars != null && foundedVars.Any())
                        seekedDataTypes.Add(foundedVars.First().DataType);
                }

                foreach (var seekedDataType in seekedDataTypes.Distinct())
                {
                    potentialVariables = node.SymbolTable.GetVariablesByType(seekedDataType, potentialVariables, SymbolTable.Scope.GlobalStorage);
                }

                potentialVariables = potentialVariables.AsQueryable().Where(variablePredicate);
            }
            else //Get all 
            {
                potentialVariables = node.SymbolTable.GetVariables(variablePredicate,SymbolTable.Scope.GlobalStorage);
            }

            foreach (var potentialVariable in potentialVariables) //Those variables could be inside a typedef or a level, we need to check to rebuild the qualified name correctly.
            {
                SearchVariableInTypesAndLevels(node, potentialVariable, ref completionItems);
            }

            if (qualifiedNameTokens.Any())
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
            IEnumerable<CompletionItem> completionItems = new List<CompletionItem>();
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(fileCompiler, codeElement);
            if(node == null)
                return completionItems;

            var tokensUntilCursor = arrangedCodeElement?.ArrangedConsumedTokens
            .Except(new List<Token>() { userFilterToken })
            .Where(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || t.Line < position.line + 1).Reverse();
            
            //Detect what's before the OF token
            var tokenBeforeOf = tokensUntilCursor?.Skip(1).FirstOrDefault(); //Skip(1) will skip the OF token

            if (tokenBeforeOf == null || !(tokenBeforeOf.TokenType == TokenType.ADDRESS || tokenBeforeOf.TokenType == TokenType.UserDefinedWord)) 
                return completionItems;

            switch (tokenBeforeOf.TokenType) //In the future, this will allow to switch between different token declared before OF. 
            {
                case TokenType.ADDRESS:
                {
                    var contextToken = tokensUntilCursor.Skip(2).FirstOrDefault(); //Try to get the token that may define the completion context
                    completionItems = GetCompletionForAddressOf(node, contextToken, userFilterText);
                    break;
                }
                case TokenType.UserDefinedWord:
                {
                    completionItems = GetCompletionForOfParent(node, tokenBeforeOf, userFilterText);
                    break;
                }
            }

            return completionItems;
        }

        /// <summary>
        /// Get completion Item for a types ADDRESS OF demand. 
        /// This method will modify the completionItems ref parameter.
        /// CompletionItems will be filtered on variables declared in LINKAGE SECTION and with LevelNumber equal to 01 or 77. 
        /// </summary>
        /// <param name="completionItems">Ref to a list of completion items</param>
        /// <param name="node">Node found on cursor position</param>
        /// <param name="contextToken">ContextToken to select if it's a SET or something else</param>
        /// <param name="userFilterText">Variable Name Filter</param>
        public static IEnumerable<CompletionItem> GetCompletionForAddressOf(Node node, Token contextToken, string userFilterText)
        {
            IEnumerable<DataDefinition> potentialVariable = null;
            if (node == null)
                return new List<CompletionItem>();

            if (contextToken != null && contextToken.TokenType == TokenType.SET)
            {
                //Get all the variables in Linkage section with Level 01 or 77 and starting by userFilterText. 
                potentialVariable = node.SymbolTable.GetVariables(v => v != null
                                                && v.IsFlagSet(Node.Flag.LinkageSectionNode)
                                                && v.CodeElement is DataDefinitionEntry
                                                && ((v.CodeElement).LevelNumber.Value == 1 || (v.CodeElement).LevelNumber.Value == 77)
                                                && v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                SymbolTable.Scope.GlobalStorage);
            }
            else 
            {
                //Get all the variables from any section with level 01 or 77 and starting by userFilterText.
                potentialVariable = node.SymbolTable.GetVariables(
                    v => v != null
                         && v.CodeElement is DataDefinitionEntry
                         && ((v.CodeElement).LevelNumber.Value == 1 ||
                             (v.CodeElement).LevelNumber.Value == 77)
                         && v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                        SymbolTable.Scope.GlobalStorage);
            }

            return CompletionFactoryHelpers.CreateCompletionItemsForVariables(potentialVariable);
        }


        public static IEnumerable<CompletionItem> GetCompletionForOfParent(Node node, Token variableNameToken,
            string userFilterText)
        {
            var completionItems = new List<CompletionItem>();
            if (node == null)
                return completionItems;

            var currentVariable = node.SymbolTable.GetVariables(
                    v => v != null && v.Name.Equals(variableNameToken.Text, StringComparison.InvariantCultureIgnoreCase), SymbolTable.Scope.Global)
                .FirstOrDefault();

            if (currentVariable == null)
                return completionItems;

            var currentParent = currentVariable.Parent as DataDefinition;
            if (currentParent != null)
            {
                completionItems.Add(CompletionFactoryHelpers.CreateCompletionItemForVariable(currentParent));
            }
            return completionItems;
        }

        #endregion



        #region Helpers

        public static IReadOnlyList<Node> GetTypeChildren(SymbolTable symbolTable, DataDefinition dataDefNode)
        {
            if (symbolTable == null || dataDefNode == null)
                return null;

            var type = symbolTable.GetTypes(
                t => t.Name.Equals(dataDefNode.DataType.Name, StringComparison.InvariantCultureIgnoreCase)
                     ||
                     t.VisualQualifiedName.ToString()
                         .Equals(dataDefNode.DataType.Name, StringComparison.InvariantCultureIgnoreCase),
                    SymbolTable.Scope.Intrinsic).FirstOrDefault();

            return type?.Children;
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
            if (!variable.IsPartOfATypeDef)  //Variable is not comming from a type. 
            {
                if (symbolTable.GetVariablesExplicit(new URI(variable.Name)).Any())   //Check if this variable is present locally. 
                {
                    completionItems.Add(CompletionFactoryHelpers.CreateCompletionItemForVariable(variable));
                }
            }
            else
            {
                if (symbolTable.TypesReferences != null) //We are in a typedef, get references of this type
                {
                    var type = variable.ParentTypeDefinition;
                    IEnumerable<DataDefinition> references = null;
                    references = symbolTable.TypesReferences.Where(t => t.Key == type).SelectMany(r => r.Value);

                    foreach (var reference in references)
                    {
                        if (symbolTable.GetVariablesExplicit(new URI(reference.Name)).Any())  //Check if this variable is present locally. If not just ignore it
                        {
                            if (reference.ParentTypeDefinition == null) //Check if the variable is inside a typedef or not, if not it's a final varaible
                            {
                                var referenceArrangedQualifiedName = string.Join("::", reference.VisualQualifiedName.ToString().Split(reference.VisualQualifiedName.Separator).Skip(1)); //Skip Program Name
                                var finalQualifiedName = string.Format("{0}::{1}", referenceArrangedQualifiedName, variable.VisualQualifiedName.Head);
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
                                        tempComp.insertText += "::" + variable.VisualQualifiedName.Head;
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

        private static void GetNextRelevantChildren(Node dataDefinition, List<Node> children)
        {
            if (string.IsNullOrEmpty(dataDefinition.Name) && dataDefinition.Children.Any())
            {
                foreach (var child in dataDefinition.Children)
                {
                    GetNextRelevantChildren(child, children);
                }
            }
            else
            {
                children.Add(dataDefinition);
            }
        }

        #endregion
    }
}

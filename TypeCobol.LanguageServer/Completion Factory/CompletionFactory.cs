using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
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
        /// <param name="codeElement">The PERFORM CodeElement</param>
        /// <param name="userFilterToken">The PERFORM token</param>
        /// <returns></returns>
        public static List<CompletionItem> GetCompletionPerformParagraph(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var performNode = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
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
                                                                           SymbolTable.Scope.Program);
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
        public static List<CompletionItem> GetCompletionForProcedure(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary)
        {
            IEnumerable<FunctionDeclaration> procedures = null;
            IEnumerable<DataDefinition> variables = null;
            var completionItems = new List<CompletionItem>();
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return completionItems;

            if (node.SymbolTable != null)
            {
                var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
                procedures = node.SymbolTable.GetFunctions(
                    f => f.VisualQualifiedName.ToString()
                             .StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase) ||
                         f.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                    SymbolTable.Scope.Intrinsic);
                variables = node.SymbolTable.GetVariables(
                    da => da.Picture != null && da.DataType == DataType.Alphanumeric &&
                          da.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                    SymbolTable.Scope.Program);
            }
            else
            {
                variables = Enumerable.Empty<DataDefinition>();
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
        public static List<CompletionItem> GetCompletionForProcedureParameter(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken, FunctionDeclaration procedureSignatureContext)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
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

            
            ISet<DataDefinition> potentialVariablesForCompletion = null;
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

                potentialVariablesForCompletion = node.SymbolTable.GetVariablesByType(parameterToFill.DataType, potentialVariablesForCompletion, SymbolTable.Scope.Program);

            }

            if (potentialVariablesForCompletion == null) return completionItems;

            //Convert potential variables into actual CompletionItems
            var variables = potentialVariablesForCompletion
                .Where(v => v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase)) //Filter on user text
                .Distinct()
                .SelectMany(d => node.SymbolTable.GetVariablesExplicitWithQualifiedName(new URI(d.Name)));
            var items = CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, fileCompiler.CompilerOptions);
            completionItems.AddRange(items);

            CompletionFactoryHelpers.Case textCase = CompletionFactoryHelpers.GetTextCase(codeElement.ConsumedTokens.First(t => t.TokenType == TokenType.CALL).Text);
            Dictionary<ParameterDescription.PassingTypes, string> paramWithCase = CompletionFactoryHelpers.GetParamsWithCase(textCase);
            //If signature of procedure is available
            if (procedureSignatureContext != null)
            {
                //Add IN-OUT or OUTPUT after INPUT ?
                if (lastSignificantToken.TokenType == TokenType.INPUT
                    && alreadyGivenParametersCount == (procedureSignatureContext.Profile.InputParameters.Count - 1))
                {
                    if (procedureSignatureContext.Profile.InoutParameters.Count != 0)
                    {
                        AddIn_OutSuffixToCompletionItems(lastSignificantToken, completionItems, paramWithCase);

                    }
                    else if (procedureSignatureContext.Profile.OutputParameters.Count != 0)
                    {
                        AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems, paramWithCase);
                    }
                }

                //Add OUTPUT after IN-OUT ?
                else if (lastSignificantToken.TokenType == TokenType.IN_OUT
                         && alreadyGivenParametersCount == (procedureSignatureContext.Profile.InoutParameters.Count - 1)
                         && procedureSignatureContext.Profile.OutputParameters.Count != 0)
                {
                    AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems, paramWithCase);
                }
            }
            else
            {
                //Add IN-OUT or OUTPUT after INPUT ?
                //If we reach the last INPUT parameter
                if (lastSignificantToken.TokenType == TokenType.INPUT && alreadyGivenParametersCount == maxInput - 1)
                {
                    //If all procs have IN-OUT params
                    if (allProcsHaveInOutParams)
                    {
                        AddIn_OutSuffixToCompletionItems(lastSignificantToken, completionItems, paramWithCase);
                    }
                    //If no procedures have IN-OUT params and all have OUTPUT params
                    else if (noProcsHaveInOutParams && allProcsHaveOutputParams)
                    {
                        AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems, paramWithCase);
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
                        AddOutputSuffixToCompletionItems(lastSignificantToken, completionItems, paramWithCase);
                    }
                }
            }

            return completionItems;
        }

        private static void AddIn_OutSuffixToCompletionItems(Token lastSignificantToken, List<CompletionItem> completionItems, Dictionary<ParameterDescription.PassingTypes, string> paramWithCase)
        {
            //Use -1, because it seems LSP start counting at 1
            var suffix = "\n" + new string(' ', lastSignificantToken.Column - 1) + paramWithCase[ParameterDescription.PassingTypes.InOut] + " ";
            completionItems.ForEach(ci => ci.insertText += suffix);
        }
        private static void AddOutputSuffixToCompletionItems(Token lastSignificantToken, List<CompletionItem> completionItems, Dictionary<ParameterDescription.PassingTypes, string> paramWithCase)
        {
            //Use -1, because it seems LSP start counting at 1
            var suffix = "\n" + new string(' ', lastSignificantToken.Column - 1) + paramWithCase[ParameterDescription.PassingTypes.Output] + " ";
            completionItems.ForEach(ci => ci.insertText += suffix);
        }


        #endregion

        #region Library Completion
        public static List<CompletionItem> GetCompletionForLibrary(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var callNode = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
            if (callNode?.SymbolTable == null)
            {
                return new List<CompletionItem>();
            }

            IEnumerable<Program> programs = callNode.SymbolTable.GetPrograms(userFilterToken != null ? userFilterToken.Text : string.Empty);
            return programs.Select(prog => new CompletionItem(prog.Name) { kind = CompletionItemKind.Module }).ToList();
        }

        #endregion

        #region Types Completion
        public static List<CompletionItem> GetCompletionForType(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken)
        {
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
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
        public static List<CompletionItem> GetCompletionForQualifiedName(Position position, FileCompiler fileCompiler, CodeElement codeElement, Token qualifiedNameSeparatorToken, Token userFilterToken, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
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
                        .Select(t => t.Text)
                        .ToArray();

               var possibleVariables = qualifiedName.Length > 0 ? node.SymbolTable.GetVariablesExplicit(new URI(qualifiedName)).ToArray() : null;

                List<DataDefinition> childrenCandidates = new List<DataDefinition>();
                if (possibleVariables != null && possibleVariables.Length > 0) 
                {
                    //Get children of a type to get completion possibilities
                    foreach (var variable in possibleVariables)
                    {
                        var children = new List<Node>();

                        //if it's a typed variable, propose 1st children of the type
                        if (variable.TypeDefinition != null)
                        {
                            children.AddRange(variable.TypeDefinition.Children);
                        }

                        //It's a variable with levels inside
                        //Note that Index are also children
                        if (variable.Children?.Count > 0)
                            children.AddRange(variable.Children);
                        //No children => nothing to do

                        var computedChildrenList = new List<Node>();
                        foreach (var child in children)
                        {
                            GetNextRelevantChildren(child, computedChildrenList);
                        }

                        FilterByUserText(computedChildrenList);
                    }
                }
                else
                {
                    //If no variable found, it could be a child declared in a typedef.
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

                        var typeChildren = GetTypeChildren(node.SymbolTable, nodeDataDef);
                        if (typeChildren != null)
                            children.AddRange(typeChildren);
                    }

                    FilterByUserText(children);
                }

                void FilterByUserText(List<Node> nodes)
                {
                    foreach (var child in nodes)
                    {
                        System.Diagnostics.Debug.Assert(child is DataDefinition);
                        var data = (DataDefinition) child;
                        if (data.Name != null && data.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase))
                        {
                            childrenCandidates.Add(data);
                        }
                    }
                }

                var variables = childrenCandidates.Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));
                var items = CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, fileCompiler.CompilerOptions, true);
                completionItems.AddRange(items);

                if (firstSignificantToken != null)
                {
                    switch (firstSignificantToken.TokenType)
                    {
                        case TokenType.CALL:
                        {
                            functionDeclarationSignatureDictionary.Clear(); //Clear to avoid key collision
                            //On CALL get possible procedures and functions in the seeked program
                            var program = node.SymbolTable.GetPrograms(userTokenToSeek.Text, true).FirstOrDefault();
                            if (program != null)
                            {
                                var procedures =
                                    program
                                        .SymbolTable.GetFunctions(
                                            f =>
                                                f.Name.StartsWith(userFilterText,
                                                    StringComparison.InvariantCultureIgnoreCase) ||
                                                f.VisualQualifiedName.ToString()
                                                    .StartsWith(userFilterText,
                                                        StringComparison.InvariantCultureIgnoreCase),
                                            SymbolTable.Scope.Program);

                                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, functionDeclarationSignatureDictionary, false));

                            }
                            break;
                        }
                        case TokenType.TYPE:
                        {
                            //On TYPE get possible public types in the seeked program
                            var program = node.SymbolTable.GetPrograms(userTokenToSeek.Text, true).FirstOrDefault();
                            if (program != null)
                            {
                                var types =
                                    program
                                        .SymbolTable.GetTypes(
                                            t =>
                                                t.Name.StartsWith(userFilterText,
                                                    StringComparison.InvariantCultureIgnoreCase),
                                                SymbolTable.Scope.Program
                                            );
                                completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForType(types, node, false));
                            }
                            break;
                        }
                    }
                }
            }

            return completionItems.Distinct().ToList();
        }
        #endregion

        #region Variable Completion
        public static List<CompletionItem> GetCompletionForVariable(FileCompiler fileCompiler, CodeElement codeElement, Func<DataDefinition, bool> predicate)
        {
            var completionItems = new List<CompletionItem>();
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return completionItems;

            var variables = node.SymbolTable.GetVariables(predicate, SymbolTable.Scope.Program).Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));
            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, fileCompiler.CompilerOptions));

            return completionItems;
        }
        #endregion

        #region TO Completion
        public static List<CompletionItem> GetCompletionForTo(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Token lastSignificantToken)
        {
            var compatibleDataTypes = new HashSet<DataType>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            if (arrangedCodeElement == null)
                return new List<CompletionItem>();
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return new List<CompletionItem>();

            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text.Trim();
            Func<DataDefinition, bool> variablePredicate =
                da =>
                    (da.CodeElement?.LevelNumber != null && da.CodeElement.LevelNumber.Value < 88) ||
                    (da.CodeElement == null && da is IndexDefinition); //Ignore variable of level 88 and file descriptions.



            DataDefinition firstSendingVar = null;


            //Warning with MOVE and SET where the logic is inverted :
            //MOVE will have node.CodeElement.StorageReads filled when completion is requested after TO
            //SET will have node.CodeElement.StorageWrites filled when completion is requested after TO

            //MOVE can fill multiples variables (move var1 to var2 var3 ) completion can then be asked after var3
            //so both node.CodeElement.StorageReads and node.CodeElement.StorageWrites can be filled when completion is asked

            //SET can apply to multiple variables at once (set var1 var2 to true)
            IDictionary<StorageArea, DataDefinition> senderDataDefinitions = node switch
            {
                Set => node.StorageAreaWritesDataDefinition,
                Move => node.StorageAreaReadsDataDefinition,
                Add => node.StorageAreaReadsDataDefinition,
                Inspect => node.StorageAreaReadsDataDefinition,
                _ => null
            };

            if (senderDataDefinitions?.Count > 0)
            {
                //TODO check other sending variable
                firstSendingVar = senderDataDefinitions.Values.First();
            }



            //TODO replace with node.CodeElement.StorageReads and/or node.CodeElement.StorageWrites
            //Look if the sending variable is compatible with Alpha / Numeric
            if (node.CodeElement is MoveSimpleStatement moveSimpleStatement)
            {
                
                var sendingItem = moveSimpleStatement.SendingItem;
                var sendingVar = moveSimpleStatement.SendingVariable;
                if (sendingItem != null)
                {
                    if (!(sendingItem is QualifiedName))
                    {
                        if (sendingItem is bool) compatibleDataTypes.Add(DataType.Boolean);
                        else if (sendingItem is double) compatibleDataTypes.Add(DataType.Numeric);
                        else if (sendingItem is string) compatibleDataTypes.Add(DataType.Alphanumeric);
                    }

                    if (sendingVar != null && sendingVar.IsLiteral && sendingItem is double)
                    {
                        switch (sendingVar.NumericValue.Token.TokenType)
                        {
                            case TokenType.ZERO:
                            case TokenType.ZEROS:
                            case TokenType.ZEROES:
                                //See #845, ZERO support Alphabetic/Numeric/Alphanumeric DataType
                                compatibleDataTypes.Add(DataType.Alphanumeric);
                                compatibleDataTypes.Add(DataType.Alphabetic);
                                break;
                        }
                    }
                }

                var tokenType = sendingVar?.RepeatedCharacterValue?.Token?.TokenType;
                switch (tokenType)
                {
                    case TokenType.SPACE:
                    case TokenType.SPACES:
                    case TokenType.QUOTE:
                    case TokenType.QUOTES:
                        compatibleDataTypes.Add(DataType.Alphanumeric);
                        compatibleDataTypes.Add(DataType.Alphabetic);
                        break;
                }
            }

            bool unsafeContext = arrangedCodeElement.ArrangedConsumedTokens.Any(t => t?.TokenType == TokenType.UNSAFE);

            //firstSendingVar is null, maybe the codeElement has not been parsed correctly by Antlr
            //e.g. CodeElement is not created for syntax "set xxx to"
            //TODO make the grammar accept it
            if (compatibleDataTypes.Count == 0 && firstSendingVar == null)
            {
                var filteredTokens = arrangedCodeElement.ArrangedConsumedTokens.SkipWhile(t => t.TokenType != TokenType.UserDefinedWord);
                bool needTailReverse = false;
                var qualifiedNameParts = new List<string>();
                foreach (var token in filteredTokens)
                {
                    if (token == lastSignificantToken) break; //No need to go further
                    if (token == userFilterToken) continue;
                    switch (token.TokenType)
                    {
                        case TokenType.QualifiedNameSeparator:
                            continue;
                        case TokenType.OF:
                        case TokenType.IN:
                            needTailReverse = true;
                            continue;
                        default:
                            qualifiedNameParts.Add(token.Text);
                            break;
                    }
                }

                if (qualifiedNameParts.Count > 0)
                {
                    if (needTailReverse) qualifiedNameParts.Reverse();
                    firstSendingVar = node.SymbolTable.GetVariablesExplicit(new URI(qualifiedNameParts)).FirstOrDefault();
                }
            }

            



            if (!unsafeContext && firstSendingVar != null && compatibleDataTypes.Count == 0)
            {
                //No compatible DataType found yet
                compatibleDataTypes.Add(firstSendingVar.DataType);

                // Add PrimitiveDataType to extend the search to compatible types, if null default to Alphanumeric
                compatibleDataTypes.Add(firstSendingVar.PrimitiveDataType ?? DataType.Alphanumeric);
            }

            IEnumerable<DataDefinition> potentialVariables;
            if (!unsafeContext && compatibleDataTypes.Count > 0) //Search for variable that match the DataType
            {
                ISet<DataDefinition> tempDataDefinitions = new HashSet<DataDefinition>();
                foreach (var compatibleDataType in compatibleDataTypes)
                {
                    tempDataDefinitions = node.SymbolTable.GetVariablesByType(compatibleDataType, tempDataDefinitions, SymbolTable.Scope.Program);
                }

                potentialVariables = tempDataDefinitions.Where(variablePredicate);
            }
            else //Either the statement is marked unsafe or we failed to find any compatible DataType, return all variables...
            {
                potentialVariables = node.SymbolTable.GetVariables(variablePredicate, SymbolTable.Scope.Program);
            }


            IEnumerable<DataDefinition> variablesWithoutSender;
            //Exclude all sending variables
            if (senderDataDefinitions != null)
            {
                variablesWithoutSender = potentialVariables.Except(senderDataDefinitions.Values);
            }
            //Exclude sending variable
            else if (firstSendingVar != null) 
            {
                variablesWithoutSender = potentialVariables.Where(v => v != firstSendingVar);
            }
            //No sending variable resolved
            else
            {
                variablesWithoutSender = potentialVariables;
            }


            var variables = variablesWithoutSender
                //Retrieve DataDefinitionPath which useful only if we need to qualify or use Type
                //TODO Completion is already fast with this call, but to improve :
                //don't do this if there is no need to qualify or let method CreateCompletionItemsForVariableSetAndDisambiguate call this if necessary
                .SelectMany(v => node.SymbolTable.GetVariablesExplicitWithQualifiedName(new URI(v.Name)));

            var items = CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, fileCompiler.CompilerOptions);
            if (userFilterText.Length > 0) //userFilterText is trimmed before
            {
                return items.Where(c => c.insertText.IndexOf(userFilterText, StringComparison.InvariantCultureIgnoreCase) >= 0).ToList();
            }

            return items;
        }
        #endregion

        #region OF Completion
        /// <summary>
        /// Get completion after OF Cobol Keyword. This method also adjust take in account the token before OF.
        /// </summary>
        /// <param name="fileCompiler"></param>
        /// <param name="codeElement"></param>
        /// <param name="userFilterToken"></param>
        /// <param name="position"></param>
        /// <returns></returns>
        public static List<CompletionItem> GetCompletionForOf(FileCompiler fileCompiler, CodeElement codeElement, Token userFilterToken, Position position)
        {
            var completionItems = new List<CompletionItem>();
            var userFilterText = userFilterToken == null ? string.Empty : userFilterToken.Text;
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
            if(node == null)
                return completionItems;

            var tokensUntilCursor = arrangedCodeElement?.ArrangedConsumedTokens
            .Except(new List<Token>() { userFilterToken })
            .Where(t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) || t.Line < position.line + 1)
            .Reverse()
            .ToList();
            
            //Detect what's before the OF token
            var tokenBeforeOf = tokensUntilCursor?.Skip(1).FirstOrDefault(); //Skip(1) will skip the OF token

            if (tokenBeforeOf == null || !(tokenBeforeOf.TokenType == TokenType.ADDRESS || tokenBeforeOf.TokenType == TokenType.UserDefinedWord)) 
                return completionItems;

            switch (tokenBeforeOf.TokenType) //In the future, this will allow to switch between different token declared before OF. 
            {
                case TokenType.ADDRESS:
                {
                    var contextToken = tokensUntilCursor.Skip(2).FirstOrDefault(); //Try to get the token that may define the completion context
                    completionItems = GetCompletionForAddressOf(node, contextToken, userFilterText, fileCompiler.CompilerOptions);
                    break;
                }
                case TokenType.UserDefinedWord:
                {
                    completionItems = GetCompletionForOfParent(node, tokenBeforeOf, userFilterText, fileCompiler.CompilerOptions);
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
        /// <param name="node">Node found on cursor position</param>
        /// <param name="contextToken">ContextToken to select if it's a SET or something else</param>
        /// <param name="userFilterText">Variable Name Filter</param>
        /// <param name="options">Current TypeCobolOptions</param>
        public static List<CompletionItem> GetCompletionForAddressOf(Node node, Token contextToken, string userFilterText, TypeCobolOptions options)
        {
            IEnumerable<DataDefinition> potentialVariables;
            if (node == null)
                return new List<CompletionItem>();

            if (contextToken != null && contextToken.TokenType == TokenType.SET)
            {
                //Get all the variables in Linkage section with Level 01 or 77 and starting by userFilterText. 
                potentialVariables = node.SymbolTable.GetVariables(v => v != null
                                                && v.IsFlagSet(Node.Flag.LinkageSectionNode)
                                                && IsRootDataItem(v)
                                                && v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                                                SymbolTable.Scope.Program);
            }
            else 
            {
                //Get all the variables from any section with level 01 or 77 and starting by userFilterText.
                potentialVariables = node.SymbolTable.GetVariables(
                    v => v != null
                         && IsRootDataItem(v)
                         && v.Name.StartsWith(userFilterText, StringComparison.InvariantCultureIgnoreCase),
                        SymbolTable.Scope.Program);
            }

            var variables = potentialVariables.Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));
            return CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, options);

            bool IsRootDataItem(DataDefinition dataDef)
            {
                var levelNumber = dataDef.CodeElement?.LevelNumber;
                return levelNumber != null && (levelNumber.Value == 1 || levelNumber.Value == 77);
            }
        }


        public static List<CompletionItem> GetCompletionForOfParent(Node node, Token variableNameToken, string userFilterText, TypeCobolOptions options)
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
                completionItems.Add(CompletionFactoryHelpers.CreateCompletionItemForSingleVariable(null, currentParent, options, true));
            }
            return completionItems;
        }

        #endregion

        #region Helpers

        private static IReadOnlyList<Node> GetTypeChildren(SymbolTable symbolTable, DataDefinition dataDefNode)
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

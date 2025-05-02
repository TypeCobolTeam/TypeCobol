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
                Move or Add or Inspect => node.StorageAreaReadsDataDefinition,
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
    }
}

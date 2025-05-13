using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForTo : CompletionContext
    {
        private readonly Token _lastSignificantToken;

        public CompletionForTo(Token userFilterToken, Token lastSignificantToken)
            : base(userFilterToken)
        {
            _lastSignificantToken = lastSignificantToken;
        }

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var compatibleDataTypes = new HashSet<DataType>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            if (arrangedCodeElement == null)
                return new List<CompletionItem>();
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return new List<CompletionItem>();

            Func<DataDefinition, bool> variablePredicate =
                da =>
                    (da.CodeElement?.LevelNumber != null && da.CodeElement.LevelNumber.Value < 88) ||
                    (da.CodeElement == null && da is IndexDefinition); //Ignore variable of level 88 and file descriptions.

            DataDefinition firstSendingVar = null;

            //Warning with MOVE and SET where the logic is inverted :
            //MOVE will have node.CodeElement.StorageReads filled when completion is requested after TO
            //SET will have node.CodeElement.StorageWrites filled when completion is requested after TO

            //MOVE can fill multiple variables (move var1 to var2 var3 ) completion can then be asked after var3
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
                    if (token == _lastSignificantToken) break; //No need to go further
                    if (token == UserFilterToken) continue;
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
                .SelectMany(v => node.SymbolTable.GetVariablesExplicitWithQualifiedName(new URI(v.Name)))
                .Where(pair => MatchesWithUserFilter(pair.Value));

            return CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, compilationUnit.CompilerOptions);
        }
    }
}

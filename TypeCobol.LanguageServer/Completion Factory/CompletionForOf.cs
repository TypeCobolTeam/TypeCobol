using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForOf : CompletionContext
    {
        /// <summary>
        /// Get completion items for OF used to define the parent variable.
        /// Beware of complex scenarios involving multiple chained OF.
        /// Chain example: firstVariable OF subGroupVariable OF variableBeforeOf OF [completion requested]
        /// </summary>
        /// <param name="node">The current node</param>
        /// <param name="variableNameBeforeOf">The token with the name of the variable before the OF to be completed</param>
        /// <param name="firstVariableName">The token with the name of the first variable in the OF chain</param>
        /// <param name="options">The compiler options</param>
        /// <returns>The completion items as a (not null) List</returns>
        private List<CompletionItem> GetCompletionForOfParent(Node node, Token variableNameBeforeOf, Token firstVariableName, TypeCobolOptions options)
        {
            var completionItems = new List<CompletionItem>();
            if (node == null)
                return completionItems;

            // Get all variables matching the name of the first variable in the OF chain
            var firstVariables = node.SymbolTable.GetVariables(
                    v => v != null && v.Name.Equals(firstVariableName.Text, StringComparison.OrdinalIgnoreCase), SymbolTable.Scope.Global);

            if (firstVariables == null)
                return completionItems;

            foreach (var firstVariable in firstVariables)
            {
                // Flag indicating when start to match parent (immediately if the variable before the OF is the first in the chain)
                bool matching = variableNameBeforeOf == firstVariableName;

                // Loop on the chain from first variable and look for matching parent
                Node parentNode = firstVariable.Parent;
                while (parentNode is DataDefinition parentDataDefinition)
                {
                    if (parentDataDefinition.Name.Equals(variableNameBeforeOf.Text, StringComparison.OrdinalIgnoreCase))
                    {
                        // Variable before OF is reached in the chain => start matching parent
                        matching = true;
                    }
                    else if (matching && MatchesWithUserFilter(parentDataDefinition))
                    {
                        completionItems.Add(CompletionFactoryHelpers.CreateCompletionItemForSingleVariable(null, parentDataDefinition, options, true));
                    }

                    parentNode = parentDataDefinition.Parent;
                }
            }

            return completionItems;
        }

        private readonly Position _position;

        public CompletionForOf(Token userFilterToken, Position position)
            : base(userFilterToken)
        {
            _position = position;
        }

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var completionItems = new List<CompletionItem>();
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return completionItems;

            var tokensUntilCursor = arrangedCodeElement?.ArrangedConsumedTokens
                .Except(new List<Token>() { UserFilterToken })
                .Where(t => (t.Line == _position.line + 1 && t.StopIndex + 1 <= _position.character) || t.Line < _position.line + 1)
                .Reverse()
                .ToList();

            //Detect what's before the OF token
            var tokenBeforeOf = tokensUntilCursor?.Skip(1).FirstOrDefault(); //Skip(1) will skip the OF token

            switch (tokenBeforeOf?.TokenType) //In the future, this will allow to switch between different token declared before OF. 
            {
                case TokenType.ADDRESS:
                {
                    var contextToken = tokensUntilCursor.Skip(2).FirstOrDefault(); //Try to get the token that may define the completion context
                    completionItems = GetCompletionForAddressOf(node, contextToken, compilationUnit.CompilerOptions);
                    break;
                }
                case TokenType.UserDefinedWord:
                    {
                        // Retrieve the first variable in the IN/OF chain
                        var tokenFirstVariable = tokensUntilCursor.TakeWhile(t => t.TokenType == TokenType.UserDefinedWord || t.TokenType == TokenType.IN || t.TokenType == TokenType.OF).LastOrDefault();

                        completionItems = GetCompletionForOfParent(node, tokenBeforeOf, tokenFirstVariable, compilationUnit.CompilerOptions);
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
        private List<CompletionItem> GetCompletionForAddressOf(Node node, Token contextToken, TypeCobolOptions options)
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
                                                && MatchesWithUserFilter(v),
                                                SymbolTable.Scope.Program);
            }
            else
            {
                //Get all the variables from any section with level 01 or 77 and starting by userFilterText.
                potentialVariables = node.SymbolTable.GetVariables(
                    v => v != null
                         && IsRootDataItem(v)
                         && MatchesWithUserFilter(v),
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
    }
}

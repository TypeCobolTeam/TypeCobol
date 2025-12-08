using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForInOrOf : CompletionContext
    {
        /// <summary>
        /// Get completion items for IN/OF used to define the parent variable.
        /// Beware of complex scenarios involving multiple chained IN/OF.
        /// Chain example: firstVariable OF subGroupVariable OF variableBeforeOf OF [completion requested]
        /// </summary>
        /// <param name="node">The current node</param>
        /// <param name="variableNameBefore">The token with the name of the variable before the IN/OF to be completed</param>
        /// <param name="firstVariableName">The token with the name of the first variable in the IN/OF chain</param>
        /// <param name="options">The compiler options</param>
        /// <returns>The completion items as a (not null) List</returns>
        private List<CompletionItem> GetCompletionForParent(Node node, Token variableNameBefore, Token firstVariableName, TypeCobolOptions options)
        {
            var completionItems = new List<CompletionItem>();
            if (node == null)
                return completionItems;

            firstVariableName = firstVariableName ?? variableNameBefore;

            // Get all variables matching the name of the first variable in the IN/OF chain
            var firstVariables = node.SymbolTable.GetVariables(
                    v => v != null && v.Name.Equals(firstVariableName.Text, StringComparison.OrdinalIgnoreCase), SymbolTable.Scope.Global);

            if (firstVariables == null)
                return completionItems;

            foreach (var firstVariable in firstVariables)
            {
                // Flag indicating when start to match parent (immediately if the variable before the IN/OF is the first in the chain)
                bool matching = variableNameBefore == firstVariableName;

                // Loop on the chain from first variable and look for matching parent
                Node parentNode = firstVariable.Parent;
                while (parentNode is DataDefinition parentDataDefinition)
                {
                    if (string.Equals(parentDataDefinition.Name, variableNameBefore.Text, StringComparison.OrdinalIgnoreCase))
                    {
                        // Variable before IN/OF is reached in the chain => start matching parent
                        matching = true;
                    }
                    else if (matching && MatchesWithUserFilter(parentDataDefinition))
                    {
                        completionItems.Add(CompletionFactoryHelpers.CreateCompletionItemForSingleVariable(null, parentDataDefinition, true, options, true, _lastSignificantTokenType));
                    }

                    parentNode = parentDataDefinition.Parent;
                }
            }

            return completionItems;
        }

        private readonly Position _position;
        private readonly TokenType _lastSignificantTokenType; // IN or OF

        public CompletionForInOrOf(Token userFilterToken, Position position, TokenType lastSignificantTokenType)
            : base(userFilterToken)
        {
            _position = position;
            _lastSignificantTokenType = lastSignificantTokenType;
        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return [];

            var tokensUntilCursor = arrangedCodeElement?.ArrangedConsumedTokens
                .Except(new List<Token>() { UserFilterToken })
                .Where(t => (t.Line == _position.line + 1 && t.StopIndex + 1 <= _position.character) || t.Line < _position.line + 1)
                .Reverse()
                .ToList();

            // Detect what's before the IN/OF token
            var tokenBefore = tokensUntilCursor?.Skip(1).FirstOrDefault(); // Skip(1) will skip the IN/OF token

            switch (tokenBefore?.TokenType) // In the future, this will allow to switch between different token declared before IN/OF
            {
                case TokenType.UserDefinedWord:
                {
                    // IN/OF is used to qualify a variable => retrieve the first variable in the IN/OF chain
                    var tokenFirstVariable = tokensUntilCursor.TakeWhile(t => t.TokenType is TokenType.UserDefinedWord or TokenType.IN or TokenType.OF).LastOrDefault();
                    if (tokenFirstVariable == null)
                    {
                        // It can happen! In this case we rely on tokenBefore to be able to return something
                        tokenFirstVariable = tokenBefore;
                    }
                    return [ GetCompletionForParent(node, tokenBefore, tokenFirstVariable, compilationUnit.CompilerOptions) ];
                }
                case TokenType.ADDRESS:
                {
                    if (_lastSignificantTokenType == TokenType.OF)
                    {
                        // Manage specifically ADDRESS OF
                        var contextToken = tokensUntilCursor.Skip(2).FirstOrDefault(); // Try to get the token that may define the completion context
                        return [ GetCompletionForAddressOf(node, contextToken, compilationUnit.CompilerOptions) ];
                    }
                    break;
                }
            }

            return [];
        }

        /// <summary>
        /// Get completion Item for a types ADDRESS OF demand.
        /// This method will modify the completionItems ref parameter.
        /// CompletionItems will be filtered on variables declared in LINKAGE SECTION and with LevelNumber equal to 01 or 77.
        /// </summary>
        /// <param name="node">Node found on cursor position</param>
        /// <param name="contextToken">ContextToken to select if it's a SET or something else</param>
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
            return CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, true, options);

            bool IsRootDataItem(DataDefinition dataDef)
            {
                var levelNumber = dataDef.CodeElement?.LevelNumber;
                return levelNumber != null && (levelNumber.Value == 1 || levelNumber.Value == 77);
            }
        }
    }
}

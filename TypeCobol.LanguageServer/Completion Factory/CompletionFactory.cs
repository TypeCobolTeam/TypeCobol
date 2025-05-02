using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
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

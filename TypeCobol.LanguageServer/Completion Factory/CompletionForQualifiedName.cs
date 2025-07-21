using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForQualifiedName : CompletionContext
    {
        private static void GetNextRelevantChildren(Node dataDefinition, List<Node> children)
        {
            if (string.IsNullOrEmpty(dataDefinition.Name) && dataDefinition.ChildrenCount > 0)
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

        private static IReadOnlyList<Node> GetTypeChildren(SymbolTable symbolTable, DataDefinition dataDefNode)
        {
            if (symbolTable == null || dataDefNode == null)
                return null;

            var type = symbolTable.GetTypes(
                t => t.Name.Equals(dataDefNode.DataType.Name, StringComparison.OrdinalIgnoreCase)
                     ||
                     t.VisualQualifiedName.ToString()
                         .Equals(dataDefNode.DataType.Name, StringComparison.OrdinalIgnoreCase),
                SymbolTable.Scope.Intrinsic).FirstOrDefault();

            return type?.Children;
        }

        private readonly Token _qualifiedNameSeparatorToken;
        private readonly Position _position;
        private readonly Dictionary<SignatureInformation, FunctionDeclaration> _functionDeclarationSignatureDictionary;

        public CompletionForQualifiedName(Token userFilterToken, Token qualifiedNameSeparatorToken, Position position, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary)
            : base(userFilterToken)
        {
            _qualifiedNameSeparatorToken = qualifiedNameSeparatorToken;
            _position = position;
            _functionDeclarationSignatureDictionary = functionDeclarationSignatureDictionary;
        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                yield break;

            //Get the token before MatchingToken 
            var userTokenToSeek =
                arrangedCodeElement?.ArrangedConsumedTokens.ElementAt(
                    arrangedCodeElement.ArrangedConsumedTokens.IndexOf(_qualifiedNameSeparatorToken) - 1);
            var qualifiedNameTokens = new List<Token>();
            if (arrangedCodeElement != null)
            {
                qualifiedNameTokens.AddRange(
                    arrangedCodeElement.ArrangedConsumedTokens?.Where(
                        t => (t?.TokenType == TokenType.UserDefinedWord || t?.TokenType == TokenType.QualifiedNameSeparator) && (t.EndColumn <= _position.character && t.Line == _position.line + 1) || t.Line < _position.line + 1));
                //Remove all the userdefinedword token and also QualifiedNameToken
                arrangedCodeElement.ArrangedConsumedTokens = arrangedCodeElement.ArrangedConsumedTokens.Except(qualifiedNameTokens).ToList();
                //We only want the token that in front of any QualifiedName 
                //Get the first significant token (i.e CALL/TYPE/...)
                CodeElementMatcher.MatchCompletionCodeElement(_position, new List<CodeElementWrapper> { arrangedCodeElement }, out _, out var firstSignificantToken);

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
                                 !(t.Text == UserFilterText && UserFilterToken != null &&
                                   t.StartIndex == UserFilterToken.StartIndex && t.EndColumn == UserFilterToken.EndColumn) &&
                                 ((firstSignificantToken != null && ((t.StartIndex >= firstSignificantToken.EndColumn && t.Line == firstSignificantToken.Line) || t.Line > firstSignificantToken.Line))
                                 || firstSignificantToken == null)
                                 && ((t.EndColumn <= _position.character && t.Line == _position.line + 1) || t.Line < _position.line + 1))
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
                                    tc => tc.Name != null && tc.Name.Equals(userTokenToSeek.Text, StringComparison.OrdinalIgnoreCase)),
                                SymbolTable.Scope.Namespace
                            );

                    foreach (var nodeType in potentialTypes.SelectMany(t => t.Children).Where(c => c != null && c.Name != null && c.Name.Equals(userTokenToSeek.Text, StringComparison.OrdinalIgnoreCase)))
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
                        var data = (DataDefinition)child;
                        if (MatchesWithUserFilter(data))
                        {
                            childrenCandidates.Add(data);
                        }
                    }
                }

                var variables = childrenCandidates.Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));
                var variableItems = CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, true, compilationUnit.CompilerOptions, true);

                if (firstSignificantToken != null)
                {
                    Func<Program, bool> matchesDeclaringProgram = program => program.Name.Equals(userTokenToSeek.Text, StringComparison.OrdinalIgnoreCase);
                    switch (firstSignificantToken.TokenType)
                    {
                        case TokenType.CALL:
                            {
                                _functionDeclarationSignatureDictionary.Clear(); //Clear to avoid key collision
                                                                                 //On CALL get possible procedures and functions in the seeked program
                                var program = node.SymbolTable.GetPrograms(matchesDeclaringProgram).FirstOrDefault();
                                if (program != null)
                                {
                                    var procedures = program.SymbolTable.GetFunctions(StartsWithUserFilter, SymbolTable.Scope.Program);
                                    yield return CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, _functionDeclarationSignatureDictionary, false);
                                }
                                break;
                            }
                        case TokenType.TYPE:
                            {
                                //On TYPE get possible public types in the seeked program
                                var program = node.SymbolTable.GetPrograms(matchesDeclaringProgram).FirstOrDefault();
                                if (program != null)
                                {
                                    var types = program.SymbolTable.GetTypes(StartsWithUserFilter, SymbolTable.Scope.Program);
                                    yield return CompletionFactoryHelpers.CreateCompletionItemsForType(types, node, false);
                                }
                                break;
                            }
                    }
                }

                yield return variableItems;
            }
        }
    }
}

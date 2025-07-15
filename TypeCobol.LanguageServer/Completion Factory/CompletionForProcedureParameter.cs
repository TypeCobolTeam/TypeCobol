using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForProcedureParameter : CompletionContext
    {
        private static void AddIn_OutSuffixToCompletionItems(Token parameterPassingDirectionToken, List<CompletionItem> completionItems, Dictionary<ParameterDescription.PassingTypes, string> paramWithCase)
        {
            //Use -1, because it seems LSP start counting at 1
            var suffix = "\n" + new string(' ', parameterPassingDirectionToken.Column - 1) + paramWithCase[ParameterDescription.PassingTypes.InOut] + " ";
            completionItems.ForEach(ci => ci.insertText += suffix);
        }

        private static void AddOutputSuffixToCompletionItems(Token parameterPassingDirectionToken, List<CompletionItem> completionItems, Dictionary<ParameterDescription.PassingTypes, string> paramWithCase)
        {
            //Use -1, because it seems LSP start counting at 1
            var suffix = "\n" + new string(' ', parameterPassingDirectionToken.Column - 1) + paramWithCase[ParameterDescription.PassingTypes.Output] + " ";
            completionItems.ForEach(ci => ci.insertText += suffix);
        }

        private readonly Token _parameterPassingDirectionToken;
        private readonly Position _position;
        private readonly FunctionDeclaration _procedureSignatureContext;

        public CompletionForProcedureParameter(Token userFilterToken, Token parameterPassingDirectionToken, Position position, FunctionDeclaration procedureSignatureContext)
            : base(userFilterToken)
        {
            _parameterPassingDirectionToken = parameterPassingDirectionToken;
            _position = position;
            _procedureSignatureContext = procedureSignatureContext;
        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var arrangedCodeElement = codeElement as CodeElementWrapper;
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return [];

            //Get procedure name or qualified name
            string procedureName = CompletionFactoryHelpers.GetProcedureNameFromTokens(arrangedCodeElement?.ArrangedConsumedTokens);
            IEnumerable<FunctionDeclaration> calledProcedures;

            if (_procedureSignatureContext == null ||
                !(_procedureSignatureContext.QualifiedName.ToString().Equals(procedureName, StringComparison.OrdinalIgnoreCase)
                  || _procedureSignatureContext.Name.Equals(procedureName, StringComparison.OrdinalIgnoreCase)))
            {
                //Try to get procedure by its name
                calledProcedures =
                    node.SymbolTable.GetFunctions(
                        p =>
                            p.Name.Equals(procedureName, StringComparison.OrdinalIgnoreCase) ||
                            p.VisualQualifiedName.ToString().Equals(procedureName, StringComparison.OrdinalIgnoreCase),
                            SymbolTable.Scope.Intrinsic
                        );
            }
            else
            {
                //If the procedure name is equivalent to the signature selected by signature help, we can assume the user is always on the same procedure. 
                calledProcedures = new List<FunctionDeclaration> { _procedureSignatureContext };
            }

            var alreadyGivenTokens = arrangedCodeElement?.ArrangedConsumedTokens
                .SkipWhile(t => t != _parameterPassingDirectionToken).Skip(1)
                .TakeWhile(t => t.TokenType != TokenType.OUTPUT && t.TokenType != TokenType.IN_OUT)
                .Except(new List<Token>() { UserFilterToken })
                .Where(t => (t.StartIndex < _position.character && t.Line == _position.line + 1) || t.Line < _position.line + 1);

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
                switch (_parameterPassingDirectionToken.TokenType)
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

                if (procedure.Profile.InputParameters.Count > maxInput)
                {
                    maxInput = procedure.Profile.InputParameters.Count;
                }
                if (procedure.Profile.InoutParameters.Count > maxInOut)
                {
                    maxInOut = procedure.Profile.InoutParameters.Count;
                }

                if (procedure.Profile.InoutParameters.Count == 0)
                {
                    allProcsHaveInOutParams = false;
                }
                else
                {
                    noProcsHaveInOutParams = false;
                }
                if (procedure.Profile.OutputParameters.Count == 0)
                {
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

            if (potentialVariablesForCompletion == null) return [];

            //Convert potential variables into actual CompletionItems
            var variables = potentialVariablesForCompletion
                .Where(MatchesWithUserFilter) //Filter on user text
                .Distinct()
                .SelectMany(d => node.SymbolTable.GetVariablesExplicitWithQualifiedName(new URI(d.Name)));
            var completionItems = CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, compilationUnit.CompilerOptions);

            Token callToken = codeElement.ConsumedTokens.First(t => t.TokenType == TokenType.CALL);
            Dictionary<ParameterDescription.PassingTypes, string> paramWithCase = CompletionFactoryHelpers.GetParamsUsingMatchingCase(callToken);
            //If signature of procedure is available
            if (_procedureSignatureContext != null)
            {
                //Add IN-OUT or OUTPUT after INPUT ?
                if (_parameterPassingDirectionToken.TokenType == TokenType.INPUT
                    && alreadyGivenParametersCount == (_procedureSignatureContext.Profile.InputParameters.Count - 1))
                {
                    if (_procedureSignatureContext.Profile.InoutParameters.Count != 0)
                    {
                        AddIn_OutSuffixToCompletionItems(_parameterPassingDirectionToken, completionItems, paramWithCase);

                    }
                    else if (_procedureSignatureContext.Profile.OutputParameters.Count != 0)
                    {
                        AddOutputSuffixToCompletionItems(_parameterPassingDirectionToken, completionItems, paramWithCase);
                    }
                }

                //Add OUTPUT after IN-OUT ?
                else if (_parameterPassingDirectionToken.TokenType == TokenType.IN_OUT
                         && alreadyGivenParametersCount == (_procedureSignatureContext.Profile.InoutParameters.Count - 1)
                         && _procedureSignatureContext.Profile.OutputParameters.Count != 0)
                {
                    AddOutputSuffixToCompletionItems(_parameterPassingDirectionToken, completionItems, paramWithCase);
                }
            }
            else
            {
                //Add IN-OUT or OUTPUT after INPUT ?
                //If we reach the last INPUT parameter
                if (_parameterPassingDirectionToken.TokenType == TokenType.INPUT && alreadyGivenParametersCount == maxInput - 1)
                {
                    //If all procs have IN-OUT params
                    if (allProcsHaveInOutParams)
                    {
                        AddIn_OutSuffixToCompletionItems(_parameterPassingDirectionToken, completionItems, paramWithCase);
                    }
                    //If no procedures have IN-OUT params and all have OUTPUT params
                    else if (noProcsHaveInOutParams && allProcsHaveOutputParams)
                    {
                        AddOutputSuffixToCompletionItems(_parameterPassingDirectionToken, completionItems, paramWithCase);
                    }
                    //Otherwise we cannot choose between IN-OUT, OUTPUT and nothing, so we choose nothing and let the user add the good keyword manually.
                    //#908 will change this behavior by asking for the signature context
                }

                //Add OUTPUT after IN-OUT ?
                else if (_parameterPassingDirectionToken.TokenType == TokenType.IN_OUT && alreadyGivenParametersCount == (maxInOut - 1))
                {
                    //If all procedures have OUTPUT parameter
                    if (allProcsHaveOutputParams)
                    {
                        AddOutputSuffixToCompletionItems(_parameterPassingDirectionToken, completionItems, paramWithCase);
                    }
                }
            }

            return [ completionItems ];
        }
    }
}

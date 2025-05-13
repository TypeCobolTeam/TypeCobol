using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.SignatureHelper
{
    public static class ProcedureSignatureHelper
    {
        /// <summary>
        /// This method will format the label of the SignatureInformation with procedure's name and all the expected arguments
        /// </summary>
        /// <param name="procedure">The selected procedure</param>
        /// <returns></returns>
        public static SignatureInformation SignatureHelperSignatureFormatter(FunctionDeclaration procedure)
        {
            var parametersInfo = new ParameterInformation[procedure.Profile.Parameters.Count];
            int i = 0; bool firstPass = true;
            foreach (var parameter in procedure.Profile.InputParameters)
            {
                var label = string.Format("{0}{1}({2})", firstPass ? "INPUT " : null, parameter.DataName, parameter.DataType.Name);
                parametersInfo[i] = new ParameterInformation() { label = label }; //Add commented documentation linked to parameter

                i++; firstPass = false;
            }
            firstPass = true;
            foreach (var parameter in procedure.Profile.InoutParameters)
            {
                var label = string.Format("{0}{1}({2})", firstPass ? "IN-OUT " : null, parameter.DataName, parameter.DataType.Name);
                parametersInfo[i] = new ParameterInformation() { label = label };

                i++; firstPass = false;
            }
            firstPass = true;
            foreach (var parameter in procedure.Profile.OutputParameters)
            {
                var label = string.Format("{0}{1}({2})", firstPass ? "OUTPUT " : null, parameter.DataName, parameter.DataType.Name);
                parametersInfo[i] = new ParameterInformation() { label = label };

                i++; firstPass = false;
            }

            return new SignatureInformation()
            {
                label = procedure.QualifiedName.ToString(),
                documentation = null, //Replace null with commented documentation linked to procedure declaration
                parameters = parametersInfo
            };
        }

        /// <summary>
        /// This method is used to determine on which argument position the user is. It's able to determine if the user is inside the INPUT/OUTPUT/IN-OUT argument section. 
        /// </summary>
        /// <param name="procedure">The detected procedure</param>
        /// <param name="wrappedCodeElement">One codeElement containing arrangedTokens concerning a procedure CALL</param>
        /// <param name="position">Cursors position in the document</param>
        /// <returns></returns>
        public static int? SignatureHelperParameterSelecter(FunctionDeclaration procedure, CodeElementWrapper wrappedCodeElement, Position position)
        {
            int activeParameter = 0;
            var closestTokenToCursor = wrappedCodeElement.ArrangedConsumedTokens.Where(
                                t => (t.Line == position.line + 1 && t.StartIndex <= position.character && t.StopIndex+1 > position.character))
                                .OrderBy(t => Math.Abs(position.character - t.StopIndex + 1)) //Allows to get the token closest to the cursor and ignoring the one where the cursor is
                                .FirstOrDefault();

            //Get the last significant token before cursor (INPUT / OUTPUT / IN-OUT)
            var closestSignificantTokenToCursor = wrappedCodeElement.ArrangedConsumedTokens.LastOrDefault(
                t => (t.TokenType == TokenType.INPUT || t.TokenType == TokenType.OUTPUT || t.TokenType == TokenType.IN_OUT) && t.Line <= position.line + 1);
                

            if (closestSignificantTokenToCursor == null)
                return null;//No argument have to be selected for now we will wait until the user typed (input/output/in-out)

            var alreadyGivenTokens = wrappedCodeElement.ArrangedConsumedTokens
                .SkipWhile(t => t != closestSignificantTokenToCursor).Skip(1)
                .TakeWhile(t => t.TokenType != TokenType.OUTPUT && t.TokenType != TokenType.IN_OUT)
                .Where(t => (t.StartIndex < position.character && t.Line == position.line + 1) || t.Line < position.line + 1);


            int alreadyGivenParametersCount = 0;
            TokenType? previousTokenType = null;
            //Loop that allows to ignore qualified name parameters. 
            foreach (var givenToken in alreadyGivenTokens)
            {
                if (givenToken.TokenType == TokenType.UserDefinedWord && (previousTokenType == null || previousTokenType.Value == TokenType.UserDefinedWord))
                    alreadyGivenParametersCount++;
                previousTokenType = givenToken.TokenType;
            }


            if (closestSignificantTokenToCursor.TokenType == TokenType.INPUT)
            {
                activeParameter = alreadyGivenParametersCount;
            }
            else if (closestSignificantTokenToCursor.TokenType == TokenType.IN_OUT)
            {
                //Count total input parameters
                //add alreadyGivenParametersCount
                activeParameter = procedure.Profile.InputParameters.Count + alreadyGivenParametersCount;
            }
            else if (closestSignificantTokenToCursor.TokenType == TokenType.OUTPUT)
            {
                //Count total input & inout parameters
                //add alreadyGivenParametersCount
                activeParameter = procedure.Profile.InputParameters.Count +
                                                procedure.Profile.InoutParameters.Count +
                                                alreadyGivenParametersCount;
            }

            if (closestTokenToCursor != null && (closestTokenToCursor.TokenType == TokenType.UserDefinedWord || closestTokenToCursor.TokenType == TokenType.QualifiedNameSeparator) &&
                position.character <= closestTokenToCursor.StopIndex + 1 && activeParameter > 0)
            {
                activeParameter--; //The cursor is still on this argument so the user is not willing the next argument. 
            }

            return activeParameter;
        }

        public static int ParametersTester(IList<ParameterDescription> parameters, List<string> parameters2, Node node)
        {
            var length = Math.Min(parameters.Count, parameters2.Count);
            if (length == 0)
                return 0;

            int weight = 0;
            for (int i = 0; i < length; i++)
            {
                var foundedVar = node.SymbolTable.GetVariablesExplicit(new URI(parameters2[i])).FirstOrDefault();
                if (foundedVar == null || !IsParameterCompatible(parameters[i], foundedVar))
                    continue; //Ignore this parameter and check for the next one

                weight++; //Each time a parameter is valid increase weight
            }

            return weight;
        }

        private static bool IsParameterCompatible(ParameterDescription parameter1, DataDefinition parameter2)
        {
            return parameter1.DataType == parameter2.DataType;
        }

        /// <summary>
        /// Re-arrange parameters tokens to get their names in URI-style.
        /// </summary>
        /// <param name="parametersTokens">Set of tokens describing the procedure parameters list for a single
        /// passing direction (INPUT, IN-OUT or OUTPUT).</param>
        /// <returns>Enumeration of parameter names.</returns>
        public static IEnumerable<string> CollectParameters(IEnumerable<Token> parametersTokens)
        {
            var aggregatedTokens = new Stack<string>();

            Token previousToken = null;
            foreach (var token in parametersTokens)
            {
                if (previousToken != null && previousToken.TokenType == TokenType.UserDefinedWord)
                {
                    if (token.TokenType != TokenType.QualifiedNameSeparator)
                    {
                        aggregatedTokens.Push(token.Text);
                    }
                    else if (previousToken.TokenType == TokenType.UserDefinedWord)
                    {
                        var retainedString = aggregatedTokens.Pop();
                        aggregatedTokens.Push(retainedString + ".");
                    }
                }
                else if (previousToken != null && previousToken.TokenType == TokenType.QualifiedNameSeparator)
                {
                    var retainedString = aggregatedTokens.Pop();
                    aggregatedTokens.Push(retainedString + token.Text);
                }

                if (previousToken == null && token.TokenType == TokenType.UserDefinedWord)
                    aggregatedTokens.Push(token.Text);

                previousToken = token;
            }

            return aggregatedTokens.Reverse();
        }
    }
}

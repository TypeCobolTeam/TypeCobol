using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    public static class CodeElementMatcher
    {
        /// <summary>
        /// Completion Tokens is a Dictionary associating a boolean for each token supported for completion operation.
        /// The boolean flag indicates whether the completion is allowed immediately after the token.
        /// For instance for PERFORM token the last position is not allowed:
        /// PERFORM
        ///        ^
        /// that is to say if the cursor is just after the M that no completion should occurs.
        /// </summary>
        private static Dictionary<TokenType, bool> _ElligibleCompletionTokens = new()
        {
            { TokenType.PERFORM, false },
            { TokenType.CALL, false },
            { TokenType.TYPE, false },
            { TokenType.QualifiedNameSeparator, true },
            { TokenType.INPUT, false },
            { TokenType.OUTPUT, false },
            { TokenType.IN_OUT, false },
            { TokenType.MOVE, false },
            { TokenType.TO, false },
            { TokenType.SET, false },
            { TokenType.OF, false },
            { TokenType.INTO, false },
            { TokenType.DISPLAY, false },
            { TokenType.IN, false },
        };

        /// <summary>
        /// This method will try to found the best significant token that code be used fo completion. It depends on the given CodeElements and Position.
        /// It will also return the CodeElement that contains the significant token detected. 
        /// </summary>
        /// <param name="position">Parameter that specified the position of the cursor in the document</param>
        /// <param name="codeElements">List of codeElements that are concerned by the completion</param>
        /// <param name="userFilterToken">Out parameter that returns a UserDefinedWork token</param>
        /// <param name="lastSignificantToken">Out parameter that returns the Significant token detected for completion</param>
        /// <returns></returns>
        public static CodeElement MatchCompletionCodeElement(Position position,
            IEnumerable<CodeElementWrapper> codeElements, out Token userFilterToken, out Token lastSignificantToken)
        {
            lastSignificantToken = null;
            CodeElement significantCodeElement = null;
            userFilterToken = null;

            var codeElementsArrangedTokensToRestore = new List<Tuple<CodeElementWrapper, List<Token>>>();

            //Filter CodeElements 
            codeElements =
                codeElements.Where(
                    c =>
                        c.ArrangedConsumedTokens.Any(
                            t =>
                                (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character) ||
                                (t.Line <= position.line + 1))).ToList();
            //Select all the code elements that have a Consumed Tokens before the cursor.
            var closestTokenToCursor =
                codeElements.Select(
                        c =>
                            c.ArrangedConsumedTokens.LastOrDefault(
                                t => (t.Line == position.line + 1 && t.StopIndex + 1 <= position.character))).Where(t => t != null)
                    .MinBy(t => Math.Abs(position.character - t.StopIndex + 1)); //Allows to get the token closest to the cursor

            if (closestTokenToCursor != null && closestTokenToCursor.Line == position.line + 1 &&
                position.character > closestTokenToCursor.StartIndex &&
                closestTokenToCursor.StopIndex + 1 >= position.character)
            //the cursor is at the end or in the middle of a token.
            {
                if (closestTokenToCursor.StopIndex + 1 == position.character && DoesTokenAllowLastPos(closestTokenToCursor))
                //Detect if token is eligible and if the cursor is at the end of the token
                {
                    //the completion has to start from this token and this codeElement
                    codeElements =
                        codeElements
                            .SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor))
                            .ToList();
                }
                else
                {
                    var tempCodeElements =
                        codeElements.TakeWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor)).ToList();

                    if (tempCodeElements.Any())
                        codeElements = tempCodeElements;

                    //The closestToken to cursor as to be added to this codeElement as a UserDefinedWord
                    if (closestTokenToCursor.TokenType != TokenType.UserDefinedWord)
                    {
                        var codeElement = codeElements.LastOrDefault();
                        if (codeElement != null)
                        {
                            //As we are altering our input, keep track of changes for later restore
                            codeElementsArrangedTokensToRestore.Add(new Tuple<CodeElementWrapper, List<Token>>(codeElement, codeElement.ArrangedConsumedTokens));
                            codeElement.ArrangedConsumedTokens = codeElement.ArrangedConsumedTokens.ConvertAll(ReplaceClosestTokenToCursor);
                        }

                        Token ReplaceClosestTokenToCursor(Token originalToken)
                        {
                            return closestTokenToCursor.Equals(originalToken)
                                ? new Token(TokenType.UserDefinedWord, closestTokenToCursor.StartIndex, closestTokenToCursor.StopIndex, closestTokenToCursor.TokensLine)
                                : originalToken;
                        }
                    }
                }
            }
            else if (closestTokenToCursor != null)
            {
                //the completion has to start from this token and this codeElement
                codeElements =
                    codeElements
                        .SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor))
                        .ToList();
            }

            foreach (var codeElement in codeElements)
            {
                var consumedTokens =
                    codeElement.ArrangedConsumedTokens.Where(
                        t =>
                            ((t.StartIndex <= position.character && t.Line <= position.line + 1) ||
                            t.Line < position.line + 1) &&
                            t.TokenFamily != TokenFamily.Whitespace)
                        .ToArray();

                var significantTokensDetected = new Stack<Token>();
                bool significantTokenChanged = false;

                if (consumedTokens.Length > 0)
                {
                    foreach (var finalToken in consumedTokens)
                    {
                        if (finalToken.StartIndex > position.character && !(finalToken.Line < position.line + 1))
                            break;

                        if (_ElligibleCompletionTokens.ContainsKey(finalToken.TokenType) &&
                            (finalToken.StopIndex + 1 <= position.character || finalToken.Line <= position.line + 1))
                        {
                            lastSignificantToken = finalToken;
                            significantTokensDetected.Push(lastSignificantToken);
                            //If everything is Ok add the final token as LastSignificantToken
                            significantCodeElement = codeElement;
                            significantTokenChanged = true;
                        }
                    }

                    if (significantTokenChanged) //In case many codeElements are analyzed
                    {
                        //Get the UserDefinedWord associated to the cursor position in the document
                        userFilterToken =
                            consumedTokens.FirstOrDefault(
                                t =>
                                    position.character <= t.StopIndex + 1 && position.character > t.StartIndex
                                    && t.Line == position.line + 1
                                    && t.TokenType == TokenType.UserDefinedWord);
                    }


                    var isCorrectTokenFound = false;
                    while (!isCorrectTokenFound && significantTokensDetected.Count >= 0)
                    {
                        //Detect if the cursor is just after the token, in this case and if bAllowLastPos is false, set 
                        if ((lastSignificantToken != null &&
                              (!DoesTokenAllowLastPos(lastSignificantToken) && lastSignificantToken.StopIndex + 1 == position.character &&
                               lastSignificantToken.Line == position.line + 1)) 
                             ||
                                 (consumedTokens.Last().TokenType == TokenType.UserDefinedWord &&
                                  !(position.character <= consumedTokens.Last().StopIndex + 1 &&
                                    position.character >= consumedTokens.Last().StartIndex)
                                  &&
                                  lastSignificantToken != null &&
                                  !(lastSignificantToken.TokenType == TokenType.INPUT ||
                                    lastSignificantToken.TokenType == TokenType.OUTPUT ||
                                    lastSignificantToken.TokenType == TokenType.IN_OUT)))
                        {
                            if (significantTokensDetected.Any())
                                lastSignificantToken = significantTokensDetected.Pop();
                            else
                                break;
                        }
                        else
                        {
                            isCorrectTokenFound = true;
                        }
                    }

                }
            }

            // Restore original ArrangedConsumedTokens if some have been modified
            foreach (var tuple in codeElementsArrangedTokensToRestore)
            {
                tuple.Item1.ArrangedConsumedTokens = tuple.Item2;
            }

            return significantCodeElement;
        }

        private static bool DoesTokenAllowLastPos(Token token) =>
            _ElligibleCompletionTokens.TryGetValue(token.TokenType, out bool allowLastPos) && allowLastPos;
    }
}

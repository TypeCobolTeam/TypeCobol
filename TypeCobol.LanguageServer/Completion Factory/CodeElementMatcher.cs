using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    public static class CodeElementMatcher
    {
        /// <summary>
        /// This method will try to found the best significant token that code be used fo completion. It depends on the given CodeELements and Position. 
        /// It will also return the CodeElemet that contains the significant token detected. 
        /// </summary>
        /// <param name="position">Parameter that specifie the position of the cursor in the document</param>
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
                    .OrderBy(t => Math.Abs(position.character - t.StopIndex + 1)) //Allows to get the token closest to the cursor
                    .FirstOrDefault();

            if (closestTokenToCursor != null && closestTokenToCursor.Line == position.line + 1 &&
                position.character > closestTokenToCursor.StartIndex &&
                closestTokenToCursor.StopIndex + 1 >= position.character)
            //the cursor is at the end or in the middle of a token. 
            {
                if (closestTokenToCursor.StopIndex + 1 == position.character &&
                    CompletionElligibleTokens.IsCompletionElligibleToken(closestTokenToCursor) && CompletionElligibleTokens.DoesTokenAllowLastPos(closestTokenToCursor))
                //Detect if token is eligible and if the cursor is at the end of the token
                {
                    //the completion has to start from this token and this codeElement
                    codeElements =
                        codeElements.ToList()
                            .SkipWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor))
                            .ToList();
                }
                else
                {
                    var tempCodeElements =
                        codeElements.TakeWhile(c => !c.ArrangedConsumedTokens.Contains(closestTokenToCursor)).ToList();

                    if (!tempCodeElements.Any())
                        //In case there is only one codeElement the TakeWhile will not be able to get the last CodeElement, so we have to do it manually.
                        tempCodeElements.Add(codeElements.FirstOrDefault());
                    else
                        codeElements = tempCodeElements;

                    //The closestToken to cursor as to be added to this codeElement as a userdefinedword
                    if (closestTokenToCursor.TokenType != TokenType.UserDefinedWord)
                    {
                        codeElements.LastOrDefault()
                            .ArrangedConsumedTokens.Add(new Token(TokenType.UserDefinedWord,
                                                        closestTokenToCursor.StartIndex,
                                                        closestTokenToCursor.StopIndex, closestTokenToCursor.TokensLine));

                        foreach (var codeElement in codeElements)
                        {
                            if (codeElement.ArrangedConsumedTokens.Contains(closestTokenToCursor) && closestTokenToCursor.TokenType != TokenType.UserDefinedWord)
                                codeElement.ArrangedConsumedTokens.Remove(closestTokenToCursor);
                        }

                    }

                }
            }
            else if (closestTokenToCursor != null)
            {
                //the completion has to start from this token and this codeElement
                codeElements =
                    codeElements.ToList()
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
                            t.TokenFamily != TokenFamily.Whitespace);

                var significantTokensDectected = new Stack<Token>();
                bool significantTokenChanged = false;

                if (consumedTokens != null && consumedTokens.Any())
                {
                    foreach (var finalToken in consumedTokens)
                    {
                        if (finalToken.StartIndex > position.character && !(finalToken.Line < position.line + 1))
                            break;

                        if (CompletionElligibleTokens.IsCompletionElligibleToken(finalToken) &&
                            (finalToken.StopIndex + 1 <= position.character || finalToken.Line <= position.line + 1))
                        {
                            lastSignificantToken = finalToken;
                            significantTokensDectected.Push(lastSignificantToken);
                            //If eveyrhing is Ok add the final token as LastSinificantToken
                            significantCodeElement = codeElement;
                            significantTokenChanged = true;
                        }
                    }

                    if (significantTokenChanged) //In case many codeElements are analysed
                    {
                        //Get the userdefinedword associated to the cursor position in the document
                        userFilterToken =
                            consumedTokens.FirstOrDefault(
                                t =>
                                    position.character <= t.StopIndex + 1 && position.character > t.StartIndex
                                    && t.Line == position.line + 1
                                    && t.TokenType == TokenType.UserDefinedWord);
                    }


                    var isCorrectTokenFound = false;
                    while (!isCorrectTokenFound && significantTokensDectected.Count >= 0)
                    {
                        //Detect if the cursor is just after the token, in this case and if bAllowLastPos is false, set 
                        if ((lastSignificantToken != null &&
                              (!CompletionElligibleTokens.DoesTokenAllowLastPos(lastSignificantToken) && lastSignificantToken.StopIndex + 1 == position.character &&
                               lastSignificantToken.Line == position.line + 1)) ||
                             (consumedTokens.LastOrDefault().TokenType == TokenType.UserDefinedWord &&
                              !(position.character <= consumedTokens.LastOrDefault().StopIndex + 1 &&
                                position.character >= consumedTokens.LastOrDefault().StartIndex)
                              &&
                              !(lastSignificantToken.TokenType == TokenType.INPUT ||
                                lastSignificantToken.TokenType == TokenType.OUTPUT ||
                                lastSignificantToken.TokenType == TokenType.IN_OUT)))
                        {
                            significantTokensDectected.Pop();
                            lastSignificantToken = significantTokensDectected.Peek();
                        }
                        else
                        {
                            isCorrectTokenFound = true;
                        }
                    }

                }
            }


            return significantCodeElement;
        }
    }
}

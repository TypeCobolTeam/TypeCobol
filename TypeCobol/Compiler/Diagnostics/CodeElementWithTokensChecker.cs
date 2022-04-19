using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Diagnostics
{
    class CodeElementWithTokensChecker
    {

        public CodeElementWithTokensChecker()
        {

        }

        public static void CheckAreaOfDeclaration(CodeElement codeElement)
        {
            if (codeElement.ConsumedTokens == null)
            {
                return;
            }

            int currentLine = codeElement.Line;
            var actualStartingArea =
                DocumentFormat.GetTextAreaTypeInCobolReferenceFormat(codeElement.ConsumedTokens.FirstOrDefault());

            bool IsFormalizedOrMultilineCommentToken(Token token)
            {
                return token.TokenFamily == TokenFamily.FormalizedCommentsFamily ||
                       token.TokenFamily == TokenFamily.MultilinesCommentsFamily;
            }

            if (codeElement.StartingArea == TextAreaType.AreaA && actualStartingArea != TextAreaType.AreaA)
            {
                DiagnosticUtils.AddError(codeElement, codeElement.ConsumedTokens.First().SourceText +
                                                      $" should begin in Area A. It was found in '{actualStartingArea}'");
            }

            if (codeElement.StartingArea == TextAreaType.AreaB)
            {
                foreach (var token in codeElement.ConsumedTokens.SkipWhile(IsFormalizedOrMultilineCommentToken))
                {
                    if (token.Line == currentLine)
                    {
                        actualStartingArea = DocumentFormat.GetTextAreaTypeInCobolReferenceFormat(token);
                        if (actualStartingArea != codeElement.StartingArea)
                        {
                            DiagnosticUtils.AddError(codeElement, token.SourceText +
                                                                  $" should begin in Area B. It was found in '{actualStartingArea}'");

                        }

                        currentLine++;
                    }
                }
            }

        }


    }
}

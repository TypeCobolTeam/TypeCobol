using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Diagnostics
{
    internal static class CodeElementWithTokensChecker
    {
        public static void CheckAreaOfDeclaration(CodeElement codeElement)
        {
            if (codeElement.ConsumedTokens == null)
            {
                return;
            }

            var expectedStartingArea = codeElement.StartingArea;
            switch (expectedStartingArea)
            {
                case TextAreaType.AreaA:
                    //First token must be in AreaA, others can be in A or B. Check first meaningful token only.
                    var firstMeaningfulToken =
                        codeElement.ConsumedTokens.FirstOrDefault(t => !IsFormalizedOrMultilineCommentToken(t));
                    if (firstMeaningfulToken != null)
                    {
                        CheckToken(firstMeaningfulToken);
                    }

                    break;
                case TextAreaType.AreaB:
                    //All tokens must be in AreaB, check every beginning token of each line
                    int currentLine = codeElement.Line;
                    foreach (var token in codeElement.ConsumedTokens.SkipWhile(IsFormalizedOrMultilineCommentToken))
                    {
                        if (token.Line == currentLine)
                        {
                            CheckToken(token);
                        }
                    }

                    break;
                default:
                    //No check required
                    return;
            }

            bool IsFormalizedOrMultilineCommentToken(Token token)
            {
                return token.TokenFamily == TokenFamily.FormalizedCommentsFamily ||
                       token.TokenFamily == TokenFamily.MultilinesCommentsFamily;
            }

            void CheckToken(Token token)
            {
                var actualStartingArea = DocumentFormat.GetTextAreaTypeInCobolReferenceFormat(token);
                if (actualStartingArea != expectedStartingArea)
                {
                    DiagnosticUtils.AddError(codeElement,
                        token.SourceText +
                        $" should begin in {expectedStartingArea}. It was found in '{actualStartingArea}'.");
                }
            }
        }
    }
}
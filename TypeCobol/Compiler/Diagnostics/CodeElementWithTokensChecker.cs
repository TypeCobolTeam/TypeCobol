using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Diagnostics
{
    internal static class CodeElementWithTokensChecker
    {
        /// <summary>
        /// Emit a diagnostic on code element if it alters the scan state.
        /// This method is meant to be used on CodeElements located inside a COPY.
        /// <see cref="CodeElementsParserStep.ParseProcessedTokensLinesChanges"/>
        /// </summary>
        /// <param name="codeElement">Non-null Code Element to test.</param>
        public static void CheckIsScanStateAlteringCodeElement(CodeElement codeElement)
        {
            switch (codeElement.Type)
            {
                case CodeElementType.DataDivisionHeader:
                    AddError("DATA DIVISION");
                    break;
                case CodeElementType.ProcedureDivisionHeader:
                    AddError("PROCEDURE DIVISION");
                    break;
                case CodeElementType.SourceComputerParagraph:
                    var sourceComputerParagraph = (SourceComputerParagraph)codeElement;
                    if (sourceComputerParagraph.DebuggingMode?.Value == true)
                    {
                        AddError("WITH DEBUGGING MODE");
                    }
                    break;
            }

            void AddError(string codeElementName)
                => DiagnosticUtils.AddError(codeElement, $"Syntax not supported: '{codeElementName}' has been found inside COPY.");
        }

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
                    //First token must be in AreaA, others on the first line can be in A or B.
                    //On second line, specifications are not clear

                    var firstMeaningfulToken =
                        codeElement.ConsumedTokens.FirstOrDefault(t => !IsFormalizedOrMultilineCommentToken(t));
                    if (firstMeaningfulToken != null)
                    {
                        CheckToken(firstMeaningfulToken, expectedStartingArea);
                    }

                    break;
                case TextAreaType.AreaB:
                    //All tokens must be in AreaB, check every first token of each line
                    var lineNumberToCheck = codeElement.Line;
                    foreach (var token in codeElement.ConsumedTokens.SkipWhile(IsFormalizedOrMultilineCommentToken))
                    {
                        if (token.Line == lineNumberToCheck)
                        {
                            lineNumberToCheck++;
                            if (token.TokensLine.Type is CobolTextLineType.Source or CobolTextLineType.Continuation
                                || (token.TokensLine.Type == CobolTextLineType.Debug && token.TokensLine.InitialScanState != null && token.TokensLine.InitialScanState.WithDebuggingMode))
                            {
                                CheckToken(token, expectedStartingArea);
                            }
                        }
                    }

                    break;
                default:
                    //No check required
                    return;
            }

            bool IsFormalizedOrMultilineCommentToken(Token token)
            {
                var scanState = token.TokensLine.InitialScanState;
                if (scanState == null) return false;
                return scanState.InsideFormalizedComment
                       || scanState.InsideMultilineComments
                       || token.TokenFamily == TokenFamily.FormalizedCommentsFamily
                       || token.TokenFamily == TokenFamily.MultilinesCommentsFamily;
            }

            void CheckToken(Token token, TextAreaType expectedTextAreaType)
            {
                var actualStartingArea = DocumentFormat.GetTextAreaTypeInCobolReferenceFormat(token);
                if (actualStartingArea != expectedTextAreaType)
                {
                    DiagnosticUtils.AddError(codeElement, token.SourceText + $" should begin in {expectedTextAreaType}. It was found in '{actualStartingArea}'.");
                }
            }
        }
    }
}
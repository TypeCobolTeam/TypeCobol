using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;

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

            void AddError(string codeElementName) => DiagnosticUtils.AddError(codeElement, $"Syntax not supported: '{codeElementName}' has been found inside COPY.");
        }
    }
}

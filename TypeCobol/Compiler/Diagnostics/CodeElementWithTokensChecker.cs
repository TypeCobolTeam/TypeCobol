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
            foreach (var token in codeElement.ConsumedTokens)
            {
                if (token.Line == currentLine)
                {
                    var actualStartingArea = DocumentFormat.GetTextAreaTypeInCobolReferenceFormat(token);
                    if (actualStartingArea != codeElement.StartingArea)
                    {
                        DiagnosticUtils.AddError(codeElement, token.SourceText +
                                                              $" should begin in '{codeElement.StartingArea}'. It was found in '{actualStartingArea}'");

                    }
                    currentLine++;
                }
            }

        }

    }
}

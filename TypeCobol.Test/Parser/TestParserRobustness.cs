using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Parser
{
    internal static class TestParserRobustness
    { 
        public static void CheckProgramCodeElements()
        {
            Diagnostic[] parserDiagnostics = null;
            var codeElements = ParseCodeElements("TITLE \"test\"\n* First move \nMOVE a b \n* Second move\n   MOVE c TO TO d ", false, out parserDiagnostics);
        }

        private static CodeElement[] ParseCodeElements(string cobolString, bool asPartOfACopy, out Diagnostic[] parserDiagnostics)
        {
            // Load text document from string
            var textDocument = new ReadOnlyTextDocument("test string", Encoding.Default, ColumnsLayout.FreeTextFormat, "");
            textDocument.LoadChars(cobolString);

            // Create a compilation project and a compiler for this document
            var typeCobolOptions = new TypeCobolOptions();
            var project = new CompilationProject("test project", ".", new[] { ".cbl", ".cpy" },
                DocumentFormat.FreeTextFormat, typeCobolOptions, null);
            var compiler = new FileCompiler(textDocument, project.SourceFileProvider, project, typeCobolOptions, asPartOfACopy, project);

            // Execute compilation - until the CodeElements phase ONLY
            compiler.CompilationResultsForProgram.UpdateTokensLines();
            compiler.CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
            compiler.CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot();
            compiler.CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot();

            // Return CodeElements and Diagnostics
            var codeElementsDocument = compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot;
            var codeElements = codeElementsDocument.CodeElements.ToArray();
            parserDiagnostics = codeElementsDocument.ParserDiagnostics.ToArray();
            return codeElements;
        }
    }    
}

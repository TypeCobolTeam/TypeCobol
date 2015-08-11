using System.Collections.Specialized;
using System.Text.RegularExpressions;
using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Scanner;

namespace TypeCobol.Test.Compiler.Parser
{
    internal static class ParserUtils
    {
        public static CompilationDocument ScanCobolFile(string relativePath, string textName, DocumentFormat documentFormat)
        {
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(relativePath));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", relativePath));
            }

            CompilationProject project = new CompilationProject("test",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, new TypeCobolOptions());

            CompilationDocument compilationDoc = new CompilationDocument(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions());
            compilationDoc.SetupDocumentProcessingPipeline(null, 0);
            compilationDoc.StartDocumentProcessing();

            return compilationDoc;
        }

        public static CompilationUnit ParseCobolFile(string textName, DocumentFormat documentFormat = null, string folder = null)
        {
            if (folder == null) folder = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar + "Samples";
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(folder));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", localDirectory.FullName));
            }
            if (documentFormat == null) documentFormat = DocumentFormat.RDZReferenceFormat;
            CompilationProject project = new CompilationProject("test",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, new TypeCobolOptions());
            CompilationUnit compilationUnit = new CompilationUnit(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions());
            compilationUnit.SetupCodeAnalysisPipeline(null, 0);
            compilationUnit.StartDocumentProcessing();

            return compilationUnit;
        }

        public static string DumpCodeElements(CompilationUnit unit)
        {
            return DumpResult(unit.SyntaxDocument.CodeElements, unit.SyntaxDocument.Diagnostics);
        }

        public static string DumpResult(IList<CodeElement> elements, IList<Diagnostic> diagnostics)
        {
            StringBuilder builder = new StringBuilder();
            builder.Append(DiagnosticsToString(diagnostics));
            builder.Append(CodeElementsToString(elements));
            return builder.ToString();
        }

        public static string DiagnosticsToString(IList<Diagnostic> diagnostics)
        {
            if (diagnostics == null || diagnostics.Count < 1) return "";
            StringBuilder builder = new StringBuilder();
            builder.AppendLine("--- Diagnostics ---");
            foreach (Diagnostic d in diagnostics)
            {
                if (d is ParserDiagnostic)
                     builder.AppendLine(((ParserDiagnostic)d).ToStringWithRuleStack());
                else builder.AppendLine(d.ToString());
            }
            return builder.ToString();
        }


        public static string CodeElementsToString(IList<CodeElement> elements)
        {
            if (elements == null || elements.Count < 1) return "";
            StringBuilder builder = new StringBuilder();
            builder.AppendLine("--- Code Elements ---");
            foreach (CodeElement e in elements) builder.AppendLine(e.ToString());
            //TODO log Diagnostics linked to codeElement directly after, to increase test readability
            return builder.ToString();
        }


        public static void CheckWithResultFile(string result, string testName)
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Compiler\Parser\ResultFiles\" + testName + ".txt")))
            {
                CheckWithResultReader(testName, result, reader);
            }
        }

        public static void CheckWithResultReader(string testName, string result, StreamReader reader)
        {
            string expectedResult = reader.ReadToEnd();

            TestUtils.compareLines(testName, result, expectedResult);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public static CompilationUnit CreateCompilationUnitForVirtualFile()
        {
            //Prepare
            var textDocument = new TextDocument("Empty doc", Encoding.Default, ColumnsLayout.FreeTextFormat, "");

            var typeCobolOptions = new TypeCobolOptions();
            var project = new CompilationProject("Empty project", ".", new[] { "*.cbl", "*.cpy" },
                DocumentFormat.FreeTextFormat.Encoding, DocumentFormat.FreeTextFormat.EndOfLineDelimiter,
                DocumentFormat.FreeTextFormat.FixedLineLength, DocumentFormat.FreeTextFormat.ColumnsLayout, typeCobolOptions);

            var compilationUnit = new CompilationUnit(textDocument,
                DocumentFormat.RDZReferenceFormat.Encoding, project.SourceFileProvider, project, typeCobolOptions);
            compilationUnit.SetupCodeAnalysisPipeline(null, 0);
            compilationUnit.StartDocumentProcessing();

            return compilationUnit;
        }
    }

    public class TestErrorListener : BaseErrorListener 
    {
        private StringBuilder errorLog;

        public TestErrorListener()
        {
            errorLog = new StringBuilder();
            ErrorCount = 0;
        }

        public int ErrorCount { get; private set; }

        public string ErrorLog
        {
            get { return errorLog.ToString(); }
        }

        public override void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            ErrorCount++;

            IList<String> stack = ((Antlr4.Runtime.Parser)recognizer).GetRuleInvocationStack();
            foreach (string ruleInvocation in stack.Reverse())
            {
                errorLog.AppendLine(ruleInvocation);
            } 
            errorLog.AppendLine("line " + line + ":" + charPositionInLine + " at " + offendingSymbol);
            errorLog.AppendLine("=> " + msg);
            errorLog.AppendLine();
        }
    }
}

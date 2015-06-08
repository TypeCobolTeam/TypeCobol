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

        public static CompilationUnit ParseCobolFile(string textName)
        {
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(@"Compiler\Parser\Samples"));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", localDirectory.FullName));
            }
            
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;
            CompilationProject project = new CompilationProject("test",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, new TypeCobolOptions());

            CompilationUnit compilationUnit = new CompilationUnit(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions());
            compilationUnit.SetupCodeAnalysisPipeline(null, 0);
            compilationUnit.StartDocumentProcessing();

            return compilationUnit;
        }

        public static string DumpCodeElements(CompilationUnit compilationUnit)
        {
            StringBuilder sb = new StringBuilder();
            if (compilationUnit.SyntaxDocument.Diagnostics != null && compilationUnit.SyntaxDocument.Diagnostics.Count > 0)
            {
                sb.AppendLine("--- Diagnostics ---");
                foreach (Diagnostic diag in compilationUnit.SyntaxDocument.Diagnostics)
                {
                    if (diag is ParserDiagnostic)
                    {
                        sb.AppendLine(((ParserDiagnostic)diag).ToStringWithRuleStack());
                    }
                    else
                    {
                        sb.AppendLine(diag.ToString());
                    }
                }
            }
            if (compilationUnit.SyntaxDocument.CodeElements != null && compilationUnit.SyntaxDocument.CodeElements.Count > 0)
            {
                sb.AppendLine("--- Code Elements ---");
                foreach (CodeElement codeElement in compilationUnit.SyntaxDocument.CodeElements)
                {
                    sb.AppendLine(codeElement.ToString());
                }
            }
            return sb.ToString();
        }

        public static void CheckWithResultFile(string result, string testName)
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Compiler\Parser\ResultFiles\" + testName + ".txt")))
            {
                string expectedResult = reader.ReadToEnd();
                if (result != expectedResult)
                {
                    throw new Exception("Code elements produced by parser in test \"" + testName + "\" don't match the expected result");
                }
            }
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

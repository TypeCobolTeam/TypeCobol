using System.IO;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Parser.Preprocessor
{
    internal static class PreprocessorUtils
    {
        public static readonly DocumentFormat Format = new DocumentFormat(Encoding.Unicode, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat);

        public static readonly string Root = "Parser" + Path.DirectorySeparatorChar + "Preprocessor";

        private static ProcessedTokensDocument GetProcessedTokensDocument(CompilationProject currentProject, string testName)
        {
            FileCompiler fileCompiler = new FileCompiler(currentProject, testName, false);
            fileCompiler.CompileOnce(ExecutionStep.Preprocessor, false, false);
            return fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot;
        }

        public static string ProcessCompilerDirectives(CompilationProject project, string testName)
        {
            ProcessedTokensDocument processedDoc = GetProcessedTokensDocument(project, testName);

            StringBuilder sbResult = new StringBuilder();
            int lineNumber = 1;
            foreach (var line in processedDoc.Lines)
            {
                sbResult.AppendLine("-- Line " + lineNumber + " --");
                sbResult.AppendLine(BuildResultString(line));
                lineNumber++;
            }
            return sbResult.ToString();
        }

        private static string BuildResultString(IProcessedTokensLine line)
        {
            StringBuilder tokensText = new StringBuilder();
            foreach (Token token in line.TokensWithCompilerDirectives)
            {
                tokensText.AppendLine(token.ToString());
            }

            StringBuilder diagnosticsText = new StringBuilder();
            if (line.PreprocessorDiagnostics != null)
            {
                foreach (Diagnostic diagnostic in line.PreprocessorDiagnostics)
                {
                    if (diagnostic.Info.Code != (int)MessageCode.FailedToLoadTextDocumentReferencedByCopyDirective)
                    {
                        diagnosticsText.AppendLine(diagnostic.ToString());
                    }
                }
            }

            return tokensText.ToString() + diagnosticsText.ToString();
        }

        public static void CheckWithDirectiveResultFile(string result, string testName)
        {
            string path = Path.Combine(Root, "DirectiveResultFiles", testName + ".txt");
            string expected = File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
            TestUtils.compareLines(path, result, expected, PlatformUtils.GetPathForProjectFile(path));
        }

        private static string ProcessTokensDocument(ProcessedTokensDocument processedDoc)
        {
            // Tokens
            StringBuilder sbTokens = new StringBuilder();
            ITokensLinesIterator tokens = processedDoc.GetProcessedTokensIterator();
            Token token = tokens.NextToken();
            if (token != Token.END_OF_FILE)
            {
                string documentPath = null;
                int lineIndex = -1;
                do
                {
                    if (tokens.DocumentPath != documentPath)
                    {
                        documentPath = tokens.DocumentPath;
                        sbTokens.AppendLine("** Document path " + documentPath + " **");
                    }
                    if (tokens.LineIndex != lineIndex)
                    {
                        lineIndex = tokens.LineIndex;
                        sbTokens.AppendLine("-- Line " + (lineIndex + 1) + " --");
                    }
                    sbTokens.AppendLine(token.ToString());
                }
                while ((token = tokens.NextToken()) != Token.END_OF_FILE);
            }

            // Errors
            StringBuilder sbDiagnostics = new StringBuilder();
            sbDiagnostics.AppendLine();
            sbDiagnostics.AppendLine("++ Preprocessor diagnostics ++");
            bool hasDiagnostic = false;
            int lineNumber = 1;
            foreach (var line in processedDoc.Lines)
            {
                if (line.PreprocessorDiagnostics != null)
                {
                    sbDiagnostics.AppendLine("-- Line " + lineNumber + " --");
                    foreach (Diagnostic diagnostic in line.PreprocessorDiagnostics)
                    {
                        hasDiagnostic = true;
                        sbDiagnostics.AppendLine(diagnostic.ToString());
                    }
                }
                lineNumber++;
            }

            return sbTokens.ToString() + (hasDiagnostic ? sbDiagnostics.ToString() : "");
        }

        public static string ProcessCopyDirectives(CompilationProject project, string name)
        {
            return ProcessTokensDocument(GetProcessedTokensDocument(project, name));
        }

        public static void CheckWithCopyResultFile(string result, string testName)
        {
            string path = Path.Combine(Root, "CopyResultFiles", testName + ".txt");
            string expected = File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
            TestUtils.compareLines(path, result, expected, PlatformUtils.GetPathForProjectFile(path));
        }

        public static string ProcessReplaceDirectives(CompilationProject project, string name)
        {
            return ProcessTokensDocument(GetProcessedTokensDocument(project, name));
        }

        public static void CheckWithReplaceResultFile(string result, string testName)
        {
            string path = Path.Combine(Root, "ReplaceResultFiles", testName + ".txt");
            string expected = File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
            TestUtils.compareLines(path, result, expected, PlatformUtils.GetPathForProjectFile(path));
        }
    }
}

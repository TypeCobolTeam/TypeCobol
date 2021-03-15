using System.IO;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Parser.Preprocessor
{
    internal static class PreprocessorUtils
    {
        private static readonly DocumentFormat _Format = new DocumentFormat(Encoding.Unicode, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat);
        private static readonly MultilineScanState _ScanState = new MultilineScanState(Encoding.Unicode);

        public static readonly string Root = "Parser" + Path.DirectorySeparatorChar +"Preprocessor";

        private static ProcessedTokensDocument GetProcessedTokensDocument(this IProcessedTokensDocumentProvider processedTokensDocumentProvider, string testName)
        {
            //This is a hack, we should create a FileCompiler instead of using this shortcut. Note that the source is considered as a copy here.
            return processedTokensDocumentProvider.GetProcessedTokensDocument(null, testName, _ScanState, null, out _);
        }

        public static CompilationProject DirectivesProject;
        public static CompilationProject CopyProject;
        public static CompilationProject ReplaceProject;

        static PreprocessorUtils()
        {
            DirectivesProject = new CompilationProject("directives",
                PlatformUtils.GetPathForProjectFile(Root + Path.DirectorySeparatorChar+"DirectiveTestFiles"), new string[] { ".cbl", ".cpy" },
                _Format, CompilerOptions, null);

            CopyProject = new CompilationProject("copy",
                PlatformUtils.GetPathForProjectFile(Root + Path.DirectorySeparatorChar + "CopyTestFiles"), new string[] { ".cbl", ".cpy" },
                _Format, CompilerOptions, null);

            ReplaceProject = new CompilationProject("replace",
                PlatformUtils.GetPathForProjectFile(Root + Path.DirectorySeparatorChar + "ReplaceTestFiles"), new string[] { ".cbl", ".cpy" },
                _Format, CompilerOptions, null);
        }

        public static TypeCobolOptions CompilerOptions = new TypeCobolOptions();

        public static string ProcessCompilerDirectives(string testName)
        {
            ProcessedTokensDocument processedDoc = DirectivesProject.GetProcessedTokensDocument(testName);

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
            string expected = System.IO.File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
            TypeCobol.Test.TestUtils.compareLines(path, result, expected, PlatformUtils.GetPathForProjectFile(path));
        }

        private static string ProcessTokensDocument(string testName, ProcessedTokensDocument processedDoc)
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

        public static string ProcessCopyDirectives(string name)
        {
            return ProcessTokensDocument(name, CopyProject.GetProcessedTokensDocument(name));
        }

        public static void CheckWithCopyResultFile(string result, string testName)
        {
            string path = Path.Combine(Root, "CopyResultFiles", testName + ".txt");
            string expected = System.IO.File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
            TypeCobol.Test.TestUtils.compareLines(path, result, expected, PlatformUtils.GetPathForProjectFile(path));
        }

        public static string ProcessReplaceDirectives(string name)
        {
            return ProcessTokensDocument(name, ReplaceProject.GetProcessedTokensDocument(name));
        }

        public static void CheckWithReplaceResultFile(string result, string testName)
        {
            string path = Path.Combine(Root, "ReplaceResultFiles", testName + ".txt");
            string expected = System.IO.File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
            TypeCobol.Test.TestUtils.compareLines(path, result, expected, PlatformUtils.GetPathForProjectFile(path));
        }

    }

}

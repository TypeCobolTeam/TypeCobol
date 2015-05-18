using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Scanner;

namespace TypeCobol.Test.Compiler.Preprocessor
{
    internal class PreprocessorUtils
    {
        public static CompilationProject DirectivesProject;
        public static CompilationProject CopyProject;
        public static CompilationProject ReplaceProject;
        
        static PreprocessorUtils()
        {
            DirectivesProject = new CompilationProject("directives",
                PlatformUtils.GetPathForProjectFile(@"Compiler\Preprocessor\DirectiveTestFiles"), new string[] { ".cbl", ".cpy" },
                Encoding.Unicode, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat, CompilerOptions);

            CopyProject = new CompilationProject("copy",
                PlatformUtils.GetPathForProjectFile(@"Compiler\Preprocessor\CopyTestFiles"), new string[] { ".cbl", ".cpy" },
                Encoding.Unicode, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat, CompilerOptions);

            ReplaceProject = new CompilationProject("replace",
                PlatformUtils.GetPathForProjectFile(@"Compiler\Preprocessor\ReplaceTestFiles"), new string[] { ".cbl", ".cpy" },
                Encoding.Unicode, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat, CompilerOptions);
        }

        public static TypeCobolOptions CompilerOptions = new TypeCobolOptions();

        public static string ProcessCompilerDirectives(string testName)
        {
            ProcessedTokensDocument processedDoc = DirectivesProject.GetProcessedTokensDocument(null, testName);

            StringBuilder sbResult = new StringBuilder();
            foreach (var line in processedDoc.ProcessedTokensLines)
            {
                sbResult.AppendLine("-- Line " + (line.TextLineMap.TextLine.LineIndex+1) + " --");
                sbResult.AppendLine(BuildResultString(line));
            }
            return sbResult.ToString();
        }

        private static string BuildResultString(ProcessedTokensLine line)
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
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Compiler\Preprocessor\DirectiveResultFiles\" + testName + ".txt")))
            {
                string expectedResult = reader.ReadToEnd();
                if (result != expectedResult)
                {
                    throw new Exception("Tokens and diagnostics produced by preprocessor in test \"" + testName + "\" don't match the expected result");
                }
            }
        }

        private static string ProcessTokensDocument(string testName, ProcessedTokensDocument processedDoc)
        {
            // Tokens
            StringBuilder sbTokens = new StringBuilder();    
            ITokensLinesIterator tokens = processedDoc.GetTokensIterator();
            Token token = tokens.NextToken();
            if(token != Token.END_OF_FILE)
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
                        sbTokens.AppendLine("-- Line " + (lineIndex+1) + " --");
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
            foreach(var line in processedDoc.ProcessedTokensLines)
            {
                if (line.PreprocessorDiagnostics != null)
                {
                    sbDiagnostics.AppendLine("-- Line " + (line.TextLineMap.TextLine.LineIndex + 1) + " --");
                    foreach (Diagnostic diagnostic in line.PreprocessorDiagnostics)
                    {
                        hasDiagnostic = true;
                        sbDiagnostics.AppendLine(diagnostic.ToString());
                    }
                }
            }

            return sbTokens.ToString() + (hasDiagnostic ? sbDiagnostics.ToString() : "");
        }

        public static string ProcessCopyDirectives(string testName)
        {
            ProcessedTokensDocument processedDoc = CopyProject.GetProcessedTokensDocument(null, testName);
            return ProcessTokensDocument(testName, processedDoc);
        }

        public static void CheckWithCopyResultFile(string result, string testName)
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Compiler\Preprocessor\CopyResultFiles\" + testName + ".txt")))
            {
                string expectedResult = reader.ReadToEnd();
                if (result != expectedResult)
                {
                    throw new Exception("Tokens and diagnostics produced by preprocessor in test \"" + testName + "\" don't match the expected result");
                }
            }
        }

        public static string ProcessReplaceDirectives(string testName)
        {
            ProcessedTokensDocument processedDoc = ReplaceProject.GetProcessedTokensDocument(null, testName);
            return ProcessTokensDocument(testName, processedDoc);
        }

        public static void CheckWithReplaceResultFile(string result, string testName)
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Compiler\Preprocessor\ReplaceResultFiles\" + testName + ".txt")))
            {
                string expectedResult = reader.ReadToEnd();
                if (result != expectedResult)
                {
                    throw new Exception("Tokens and diagnostics produced by preprocessor in test \"" + testName + "\" don't match the expected result");
                }
            }
        }
    }
}

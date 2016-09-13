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

	public static void CheckWithDirectiveResultFile(string result, string testName) {
		string path = Path.Combine("Compiler","Preprocessor","DirectiveResultFiles",testName+".txt");
		string expected = System.IO.File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
		TypeCobol.Test.TestUtils.compareLines(path, result, expected);
	}

        private static string ProcessTokensDocument(string testName, ProcessedTokensDocument processedDoc)
        {
            // Tokens
            StringBuilder sbTokens = new StringBuilder();    
            ITokensLinesIterator tokens = processedDoc.ProcessedTokens;
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
            int lineNumber = 1;
            foreach(var line in processedDoc.Lines)
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

	public static string ProcessCopyDirectives(string name) {
		return ProcessTokensDocument(name, CopyProject.GetProcessedTokensDocument(null, name));
	}

	public static void CheckWithCopyResultFile(string result, string testName) {
		string path = Path.Combine("Compiler","Preprocessor","CopyResultFiles",testName+".txt");
		string expected = System.IO.File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
		TypeCobol.Test.TestUtils.compareLines(path, result, expected);
	}

	public static string ProcessReplaceDirectives(string name) {
		return ProcessTokensDocument(name, ReplaceProject.GetProcessedTokensDocument(null, name));
	}

	public static void CheckWithReplaceResultFile(string result, string testName) {
		string path = Path.Combine("Compiler","Preprocessor","ReplaceResultFiles",testName+".txt");
		string expected = System.IO.File.ReadAllText(PlatformUtils.GetPathForProjectFile(path));
		TypeCobol.Test.TestUtils.compareLines(path, result, expected);
	}

	}

}

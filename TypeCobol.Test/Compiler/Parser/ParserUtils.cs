﻿using System.Collections.Specialized;
using System.Text.RegularExpressions;
using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
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

            FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions(), true);
            compiler.CompileOnce();

            return compiler.CompilationResultsForCopy;
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
            FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions(), false);
            compiler.CompileOnce();

            return compiler.CompilationResultsForProgram;
        }

        public static CompilationUnit ParseCobolString(string cobolString)
        {
            //Prepare
            var textDocument = new ReadOnlyTextDocument("Empty doc", Encoding.Default, ColumnsLayout.FreeTextFormat, "");
            textDocument.LoadChars(cobolString);

            var typeCobolOptions = new TypeCobolOptions();
            var project = new CompilationProject("Empty project", ".", new[] { "*.cbl", "*.cpy" },
                DocumentFormat.FreeTextFormat.Encoding, DocumentFormat.FreeTextFormat.EndOfLineDelimiter,
                DocumentFormat.FreeTextFormat.FixedLineLength, DocumentFormat.FreeTextFormat.ColumnsLayout, typeCobolOptions);

            var compiler = new FileCompiler(textDocument, project.SourceFileProvider, project, typeCobolOptions, false);
            compiler.CompileOnce();

            return compiler.CompilationResultsForProgram;
        }

        public static string DumpCodeElements(CodeElementsDocument result)
        {
            return DumpResult(result.CodeElements, result.ParserDiagnostics);
        }

        public static string DumpResult(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics)
        {
            StringBuilder builder = new StringBuilder();
            builder.Append(DiagnosticsToString(diagnostics));
            builder.Append(CodeElementsToString(elements));
            return builder.ToString();
        }

        public static string DiagnosticsToString(IEnumerable<Diagnostic> diagnostics)
        {
            StringBuilder builder = new StringBuilder();
            bool hasDiagnostic = false;
            foreach (Diagnostic d in diagnostics)
            {
                if (d is ParserDiagnostic)
                {
                    builder.AppendLine(((ParserDiagnostic)d).ToStringWithRuleStack());
                }
                else
                {
                    builder.AppendLine(d.ToString());
                }
                hasDiagnostic = true;
            }
            if(hasDiagnostic)
            {
                builder.Insert(0, "--- Diagnostics ---" + Environment.NewLine);
                return builder.ToString();
            }
            else
            {
                return String.Empty;
            }
        }


        public static string CodeElementsToString(IEnumerable<CodeElement> elements)
        {
            StringBuilder builder = new StringBuilder();
            bool hasCodeElement = false;
            foreach (CodeElement e in elements)
            {
                builder.AppendLine(e.ToString());
                hasCodeElement = true;
            }
            //TODO log Diagnostics linked to codeElement directly after, to increase test readability
            if (hasCodeElement)
            {
                builder.Insert(0, "--- Code Elements ---" + Environment.NewLine);
                return builder.ToString();
            }
            else
            {
                return String.Empty;
            }
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

using System.Collections.Specialized;
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
using TypeCobol.Compiler.CodeModel;

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
                //First use *.cpy as tests will use file WITH extension for program but without extension for copy inside programs => small perf gain
                localDirectory.FullName, new string[] {"*.cpy", "*.cbl" },
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

        internal static string DumpResult(Program program, Class cls, IList<Diagnostic> diagnostics)
        {
            StringBuilder builder = new StringBuilder();
            if (diagnostics != null && diagnostics.Count > 0) {
                builder.AppendLine(DiagnosticsToString(diagnostics));
            }
            if (program != null) {
                builder.AppendLine("--- Program ---");
                Dump(builder, program);
            }
            if (cls != null) {
                builder.AppendLine("--- Class ---");
                Dump(builder, cls);
            }
            return builder.ToString();
        }

        internal static StringBuilder Dump(StringBuilder str, TypeCobol.Compiler.CodeModel.Program program)
        {
            str.Append("PROGRAM: ");
            Dump(str, program.Identification);
            str.AppendLine();
            Dump(str, program.Data);
            return str;
        }

        internal static StringBuilder Dump(StringBuilder str, ProgramIdentification program)
        {
            if (program == null) str.Append("?");
            else {
                str.Append(program.ProgramName);
                str.Append(" common:");    Dump(str, program.IsCommon);
                str.Append(" initial:");   Dump(str, program.IsInitial);
                str.Append(" recursive:"); Dump(str, program.IsRecursive);
                str.AppendLine();
                Dump(str, program.AuthoringProperties);
            }
            return str;
        }

        private static StringBuilder Dump(StringBuilder str, AuthoringProperties data)
        {
            if (data == null) str.Append("?");
            else {
                str.Append(" author: "); Dump(str, data.Author);
                str.Append(" written: "); Dump(str, data.DateWritten);
                str.Append(" compiled: "); Dump(str, data.DateCompiled);
                str.Append(" installation: "); Dump(str, data.Installation);
                str.Append(" security: "); Dump(str, data.Security);
            }
            return str;
        }

        private static StringBuilder Dump<T>(StringBuilder str, SyntaxProperty<T> data)
        {
            if (data == null) str.Append("?");
            else str.Append(data.Value);
            return str;
        }

        internal static StringBuilder Dump(StringBuilder builder, Class cls)
        {
            throw new NotImplementedException("TODO");
        }

        internal static StringBuilder Dump(StringBuilder str, SyntaxBoolean b)
        {
            if (b == null) str.Append("?");
            else str.Append(b.Value);
            return str;
        }

        private static StringBuilder Dump(StringBuilder str, SymbolTable table, string header=null)
        {
            if (table == null) return str;
            if (header == null) header = "SYMBOL TABLE:\n";
            if (table.CurrentScope == SymbolTable.Scope.External) header = "EXTERNAL SCOPE:\n";
            if (table.CurrentScope == SymbolTable.Scope.Global) header = "GLOBAL SCOPE:\n";
            Dictionary<string,List<DataDescriptionEntry>> map = table.DataEntries;
            if(map.Count > 0) {
                str.Append(header);
                Dump(str, map);
            }
            Dump(str, table.EnclosingScope, "ENCLOSING SCOPE:\n");
            return str;
        }

        private static void Dump(StringBuilder str, Dictionary<string, List<DataDescriptionEntry>> map) {
            foreach(string key in map.Keys) {
                foreach (var data in map[key]) {
                    Dump(str, data, 1);
                    str.Append("\n");
                }
            }
        }

        private static StringBuilder Dump(StringBuilder str, DataDescriptionEntry data, int indent = 0)
        {
            DumpIndent(str, indent);
            str.Append(data);
            return str;
        }

        private static StringBuilder DumpIndent(StringBuilder str, int indent)
        {
            for (int c=0; c<indent; c++) str.Append("  ");
            return str;
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

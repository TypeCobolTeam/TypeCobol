using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using TypeCobol.Compiler;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using String = System.String;

namespace TypeCobol.Test.Utils
{
    public static class ParserUtils
    {
        public static CompilationDocument ScanCobolFile(string relativePath, string textName, DocumentFormat documentFormat)
        {
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(relativePath));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", relativePath));
            }

            CompilationProject project = new CompilationProject("test",
                localDirectory.FullName, new string[] { ".cbl", ".cpy" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, new TypeCobolOptions());

            FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions(), null, true, project);
            compiler.CompileOnce();

            return compiler.CompilationResultsForCopy;
        }

        public static CompilationUnit ParseCobolFile(string textName, DocumentFormat documentFormat = null, string folder = null, ExecutionStep execToStep = ExecutionStep.SemanticCheck)
        {
            if (folder == null) folder = "Parser" + Path.DirectorySeparatorChar + "CodeElements";
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(folder));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", localDirectory.FullName));
            }
            if (documentFormat == null) documentFormat = DocumentFormat.RDZReferenceFormat;

            TypeCobolOptions options = new TypeCobolOptions { ExecToStep = execToStep }; //Create CompilerOptions. ExecToStep / AutoRemarks / HaltOnMissingCopy have to be set here.
            CompilationProject project = new CompilationProject("test",
                //First use *.cpy as tests will use file WITH extension for program but without extension for copy inside programs => small perf gain
                localDirectory.FullName, new string[] { ".cpy", ".cbl" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, options);
            FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, options, null, false, project);
            compiler.CompileOnce();

            return compiler.CompilationResultsForProgram;
        }

        public static CompilationUnit ParseCobolString(string cobolString)
        {
            //Prepare
            var textDocument = new ReadOnlyTextDocument("Empty doc", Encoding.Default, ColumnsLayout.FreeTextFormat, "");
            textDocument.LoadChars(cobolString);

            var typeCobolOptions = new TypeCobolOptions();
            var project = new CompilationProject("Empty project", ".", new[] { ".cbl", ".cpy" },
                DocumentFormat.FreeTextFormat.Encoding, DocumentFormat.FreeTextFormat.EndOfLineDelimiter,
                DocumentFormat.FreeTextFormat.FixedLineLength, DocumentFormat.FreeTextFormat.ColumnsLayout, typeCobolOptions);

            var compiler = new FileCompiler(textDocument, project.SourceFileProvider, project, typeCobolOptions, false, project);
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

        public static string DiagnosticsToString(IEnumerable<Diagnostic> diagnostics, bool printDiagnosticLine = true)
        {
            StringBuilder builder = new StringBuilder();
            //Sort diagnostics by line order
            foreach (Diagnostic d in diagnostics.OrderBy(d => d.Line))
            {
                string errmsg = null;

                if (d is ParserDiagnostic)
                {
                    var diagnostic = d as ParserDiagnostic;
                    errmsg = diagnostic.ToStringWithRuleStack();
                }
                else if (d is CupParserDiagnostic)
                {
                    var diagnostic = d as CupParserDiagnostic;
                    errmsg = diagnostic.ToStringWithRuleStack();
                }
                else
                {
                    errmsg = d.ToString();
                }

                builder.AppendLine(errmsg);
            }
            if (builder.Length > 0)
            {
                if (printDiagnosticLine)
                {
                    builder.Insert(0, "--- Diagnostics ---" + Environment.NewLine);
                }
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

        internal static string DumpResult(IEnumerable<Program> programs, IEnumerable<TypeCobol.Compiler.Nodes.Class> classes, IList<Diagnostic> diagnostics)
        {
            StringBuilder builder = new StringBuilder();
            if (diagnostics != null && diagnostics.Count > 0)
            {
                builder.AppendLine(DiagnosticsToString(diagnostics));
            }
            if (programs != null)
            {
                foreach (var program in programs)
                {
                    builder.AppendLine("--- Program ---");
                    Dump(builder, program);
                }
            }
            if (classes != null)
            {
                foreach (var cls in classes)
                {
                    builder.AppendLine("--- Class ---");
                    Dump(builder, cls);
                }
            }
            return builder.ToString();
        }

        internal static StringBuilder Dump(StringBuilder str, TypeCobol.Compiler.CodeModel.Program program)
        {
            str.Append("PROGRAM: ");
            Dump(str, program.Identification);
            str.AppendLine();
            str.AppendLine(program.SymbolTable.ToString(true));
            return str;
        }

        internal static StringBuilder Dump(StringBuilder str, ProgramIdentification program)
        {
            if (program == null) str.Append("?");
            else
            {
                str.Append(program.ProgramName);
                str.Append(" common:").Append(program.IsCommon);
                str.Append(" initial:").Append(program.IsInitial);
                str.Append(" recursive:").Append(program.IsRecursive);
                str.AppendLine();
                Dump(str, program.AuthoringProperties);
            }
            return str;
        }

        private static StringBuilder Dump(StringBuilder str, AuthoringProperties data)
        {
            if (data == null)
            {
                str.Append("?");
                return str;
            }
            str.Append(" author: ");
            if (data.Author.Length > 0)
            {
                foreach (var value in data.Author) str.Append(value.Value + ",");
                str.Length -= 1;
            }
            else str.Append('?');
            str.Append(" written: ");
            if (data.DateWritten.Length > 0)
            {
                foreach (var value in data.DateWritten) str.Append(value.Value + ",");
                str.Length -= 1;
            }
            else str.Append('?');
            str.Append(" compiled: ");
            if (data.DateCompiled.Length > 0)
            {
                foreach (var value in data.DateCompiled) str.Append(value.Value + ",");
                str.Length -= 1;
            }
            else str.Append('?');
            str.Append(" installation: ");
            if (data.Installation.Length > 0)
            {
                foreach (var value in data.Installation) str.Append(value.Value + ",");
                str.Length -= 1;
            }
            else str.Append('?');
            str.Append(" security: ");
            if (data.Security.Length > 0)
            {
                foreach (var value in data.Security) str.Append(value.Value + ",");
                str.Length -= 1;
            }
            else str.Append('?');
            return str;
        }

        private static StringBuilder Dump<T>(StringBuilder str, SyntaxProperty<T> data)
        {
            if (data == null) str.Append("?");
            else str.Append(data.Value);
            return str;
        }

        internal static StringBuilder Dump(StringBuilder builder, TypeCobol.Compiler.Nodes.Class cls)
        {
            throw new NotImplementedException("TODO");
        }

        // [TYPECOBOL]
        private static void Dump(StringBuilder str, IEnumerable<TypeCobol.Compiler.Nodes.TypeDefinition> typedefs)
        {
            int c = 0;
            string header = "CUSTOM TYPES:\n";
            str.Append(header);
            foreach (var typedef in typedefs)
            {
                str.Append(" * ").AppendLine(typedef.DataType.Name);
                foreach (var sub in typedef.Children)
                    DumpInTypeDef(str, (TypeCobol.Compiler.Nodes.DataDescription)sub, 2);
                c++;
            }
            if (c == 0) str.Length -= header.Length;
        }
        private static void DumpInTypeDef(StringBuilder str, TypeCobol.Compiler.Nodes.DataDescription node, int indent)
        {
            for (int i = 0; i < indent; i++) str.Append("  ");
            str.Append(" - ").AppendLine(node.CodeElement.ToString());
            foreach (var sub in node.Children)
                DumpInTypeDef(str, (TypeCobol.Compiler.Nodes.DataDescription)sub, indent + 1);
        }

        /*TODO#249
                private static void Dump(StringBuilder str, IList<Function> functions) {
                    if (functions == null || functions.Count < 1) return;
                    str.AppendLine("FUNCTIONS:");
                    foreach(var function in functions) {
                        str.Append(" £ ").Append(function.Name).Append(':').Append(function.Visibility);
                        str.AppendLine();
                        foreach(var parameter in function.Profile.InputParameters) {
                            str.Append("        in: ");
                            Dump(str, parameter);
                            str.AppendLine();
                        }
                        foreach(var parameter in function.Profile.OutputParameters) {
                            str.Append("       out: ");
                            Dump(str, parameter);
                            str.AppendLine();
                        }
                        foreach(var parameter in function.Profile.InoutParameters) {
                            str.Append("        io: ");
                            Dump(str, parameter);
                            str.AppendLine();
                        }
                        if (function.Profile.ReturningParameter != null) {
                            str.Append("    return: ");
                            Dump(str, function.Profile.ReturningParameter);
                            str.AppendLine();
                        }
                    }
                }
                private static void Dump(StringBuilder str, ParameterDescription parameter) {
                    str.Append(parameter.Name).Append(':');
                    var entry = (ParameterDescriptionEntry)parameter.CodeElement;
                    if (entry.CustomType != null) str.Append(entry.CustomType);
                    else
                    if (entry.Picture != null) str.Append(entry.Picture);
                    else str.Append("?");
                }
        // [/TYPECOBOL]

        private static void Dump(StringBuilder str, Dictionary<string, List<Named>> map) {
        foreach(string key in map.Keys) {
            foreach (var data in map[key]) {
                Dump(str, data, 1);
                str.Append("\n");
            }
        }
        }

        private static StringBuilder Dump(StringBuilder str, Named data, int indent = 0)
        {
        DumpIndent(str, indent);
        str.Append(data.Name);
        return str;
        }

        private static StringBuilder DumpIndent(StringBuilder str, int indent)
        {
        for (int c=0; c<indent; c++) str.Append("  ");
        return str;
        }
        */

        public static void CheckWithResultFile(string result, string testName)
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Parser\CodeElements\" + testName + ".txt")))
            {
                CheckWithResultReader(testName, result, reader, PlatformUtils.GetPathForProjectFile(@"Parser\CodeElements\" + testName + ".txt"));
            };
        }


        public static void CheckWithResultReader(string testName, string result, StreamReader reader, string expectedResultPath)
        {
            string expectedResult = reader.ReadToEnd();
            reader.Close();
            TestUtils.compareLines(testName, result, expectedResult, expectedResultPath);
        }


        public static List<IDocumentable> GetDocumentedNodes(Node root)
        {
            List<IDocumentable> documentedNodes = new List<IDocumentable>();

            foreach (var child in root.Children)
            {
                var documentedChild = child as IDocumentable;
                if (documentedChild != null)
                    documentedNodes.Add(documentedChild);
                documentedNodes.AddRange(GetDocumentedNodes(child));
            }
            return documentedNodes;
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

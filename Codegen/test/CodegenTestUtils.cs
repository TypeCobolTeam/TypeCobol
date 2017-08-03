using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
// DocumentFormat
using TypeCobol.Tools; // CodeElementDiagnostics

namespace TypeCobol.Codegen {

    
    public class CodegenTestUtils {
        private const string ROOT = "resources";
        private const string CONFIG = "config";
        private const string INPUT = "input";
        private const string OUTPUT = "output";


        public static void ParseGenerateCompare(string path, string skeletonPath, bool autoRemarks = false) {
            ParseGenerateCompare(path, ParseConfig(skeletonPath), autoRemarks);
        }

        /// <summary>
        /// Parse and generate using DocumentFormat.RDZReferenceFormat by default, because it's our only real target 
        /// for now and we don't have specifications for FreeFormat.
        /// </summary>
        /// <param name="path"></param>
        /// <param name="skeletons"></param>
        public static void ParseGenerateCompare(string path, List<Skeleton> skeletons = null, bool autoRemarks = false) {
            ParseGenerateCompare(path, skeletons, DocumentFormat.RDZReferenceFormat, autoRemarks);
        }
        public static void ParseGenerateCompare(string path, List<Skeleton> skeletons, DocumentFormat format, bool autoRemarks = false) {
            var document = Parser.Parse(Path.Combine(ROOT, INPUT, path), format, autoRemarks);
            var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
            var writer = new StringWriter();
            // write parsing errors
            WriteErrors(writer, document.Results.AllDiagnostics(), columns);
            // write generated code
            var codegen = new Generators.DefaultGenerator(document.Results, writer, skeletons);
            try {
                codegen.Generate(document.Results, columns);
                if (codegen.Diagnostics != null)
                    WriteErrors(writer, codegen.Diagnostics, columns);
            } finally {
                // flush
                writer.Close();
            }

            // compare with expected result
            string expected = File.ReadAllText(Path.Combine(ROOT, OUTPUT, path), format.Encoding);
            TypeCobol.Test.TestUtils.compareLines(path, writer.ToString(), expected);
        }

        private static void WriteErrors(TextWriter writer, ICollection<Diagnostic> errors,
            Compiler.Text.ColumnsLayout columns) {
            string comment = GetComment(columns);
            if (errors.Count > 0) {
                writer.WriteLine(comment + " " + errors.Count + " errors");
                foreach (var error in errors)
                    writer.WriteLine(comment + " " + error);
            }
        }

        private static string GetComment(Compiler.Text.ColumnsLayout columns) {
            if (columns == Compiler.Text.ColumnsLayout.CobolReferenceFormat)
                return "      *";
            return "*";
        }


        public static List<Skeleton> ParseConfig(string resource) {
            var path = Path.Combine(ROOT, CONFIG, resource);
            return Config.Config.Parse(path);
        }
    }
}
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler; // DocumentFormat
using TypeCobol.Tools; // CodeElementDiagnostics

namespace TypeCobol.Codegen {

    
    public class CodegenTestUtils {
        private const string ROOT = "resources";
        private const string CONFIG = "config";
        private const string INPUT = "input";
        private const string OUTPUT = "output";


        public static void ParseGenerateCompare(string path, string skeletonPath) {
            ParseGenerateCompare(path, ParseConfig(skeletonPath));
        }

        /// <summary>
        /// Parse and generate using DocumentFormat.RDZReferenceFormat by default, because it's our only real target 
        /// for now and we don't have specifications for FreeFormat.
        /// </summary>
        /// <param name="path"></param>
        /// <param name="skeletons"></param>
        public static void ParseGenerateCompare(string path, List<Skeleton> skeletons = null) {
            ParseGenerateCompare(path, skeletons, DocumentFormat.RDZReferenceFormat);
        }
        public static void ParseGenerateCompare(string path, List<Skeleton> skeletons, DocumentFormat format) {
            var document = Parser.Parse(Path.Combine(ROOT, INPUT, path), format);
            var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
            var writer = new StringWriter();
            // write parsing errors
            WriteErrors(writer, document.Errors[0], "CodeElements", columns);
            WriteErrors(writer, document.Errors[1], "ProgramClass", columns);
            // write generated code
            var codegen = new TypeCobol.Codegen.Generators.DefaultGenerator(document.Results, writer, skeletons);
            var program = document.Results.ProgramClassDocumentSnapshot.Program;

            codegen.Generate(program == null ? null : program.SyntaxTree.Root, program == null ? null : program.SymbolTable, columns);
            // flush
            writer.Close();

            // compare with expected result
            string expected = File.ReadAllText(Path.Combine(ROOT, OUTPUT, path), format.Encoding);
            TypeCobol.Test.TestUtils.compareLines(path, writer.ToString(), expected);
        }

        private static void WriteErrors(TextWriter writer, ICollection<Diagnostic> errors, string type,
            Compiler.Text.ColumnsLayout columns) {
            string comment = GetComment(columns);
            if (errors.Count > 0) {
                writer.WriteLine(comment + " " + errors.Count + " " + type + " errors");
                foreach (var error in errors)
                    writer.WriteLine(comment + " " + error);
            }
        }

        private static string GetComment(Compiler.Text.ColumnsLayout columns) {
            if (columns == Compiler.Text.ColumnsLayout.CobolReferenceFormat)
                return "      *";
            return "*";
        }


        public static List<Skeleton> ParseConfig(string resource)
        {
            var path = Path.Combine(ROOT, CONFIG, resource);
            var parser = Config.Config.CreateParser(Path.GetExtension(path));
            return parser.Parse(path);
        }
    }
}
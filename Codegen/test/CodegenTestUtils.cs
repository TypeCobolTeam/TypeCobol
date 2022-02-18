using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
#if EUROINFO_RULES
using TypeCobol.Compiler.Preprocessor;
#endif
using TypeCobol.Test;

namespace TypeCobol.Codegen {

    
    public class CodegenTestUtils {
        private const string ROOT = "resources";
        private const string INPUT = "input";
        private const string OUTPUT = "output";

        /// <summary>
        /// Parse and generate using DocumentFormat.RDZReferenceFormat by default, because it's our only real target 
        /// for now and we don't have specifications for FreeFormat.
        /// </summary>
        /// <param name="path">Path to file to parse</param>
        /// <param name="enableRemarksParsingAndGeneration">Enable parsing of REMARKS directive and its auto generation</param>
        /// <param name="typeCobolVersion">TypeCobol parser version to write in generated header</param>
        /// <param name="copies">List of copy folders</param>
        public static void ParseGenerateCompare(string path, bool enableRemarksParsingAndGeneration = false, string typeCobolVersion = null, IList<string> copies = null
#if EUROINFO_RULES
            , string cpyCopyNamesMapFilePath = null
#endif
        )
        {
            var options = new TypeCobolOptions() {
                                                     OptimizeWhitespaceScanning = false,
                                                     UseEuroInformationLegacyReplacingSyntax = enableRemarksParsingAndGeneration
                                                 };
#if EUROINFO_RULES
            if (cpyCopyNamesMapFilePath != null) options.CpyCopyNameMap = new CopyNameMapFile(cpyCopyNamesMapFilePath);
#endif
            ParseGenerateCompare(path, options, DocumentFormat.RDZReferenceFormat, typeCobolVersion, copies, null);
        }

        private static void ParseGenerateCompare(string path, TypeCobolOptions options, DocumentFormat format, string typeCobolVersion, IList<string> copies, MemoryStream lmStream)
        {
            var document = Parser.Parse(Path.Combine(ROOT, INPUT, path), false, options, format, copies);
            var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
            var writer = new StringWriter();
            // write parsing errors
            WriteErrors(writer, document.Results.AllDiagnostics(), columns);
            // write generated code
            var generatedCobolStringBuilder = new StringBuilder();
            Generator codegen;
            if (lmStream != null)
                codegen = new Generators.DefaultGeneratorWithLineMap(document.Results, generatedCobolStringBuilder, typeCobolVersion);
            else 
                codegen = new Generators.DefaultGenerator(document.Results, generatedCobolStringBuilder, typeCobolVersion);

            try {
                codegen.Generate(document.Results, columns);
                if (codegen.Diagnostics != null)
                    WriteErrors(writer, codegen.Diagnostics, columns);

                if (lmStream != null)
                {
                    codegen.GenerateLineMapFile(lmStream);
                }
            } finally {
                writer.Write(generatedCobolStringBuilder);
                // flush
                writer.Close();
            }

            // compare with expected result
            string expectedPath;
#if EUROINFO_RULES
            expectedPath = Path.Combine(ROOT, OUTPUT, path);
#else
            //Use 'NoZ' variant for expected result without ZCALLPGM (only if it exists)
            var parts = path.Split(Path.DirectorySeparatorChar).ToList();
            parts.Insert(1, "NoZ");
            string alternatePath = string.Join(Path.DirectorySeparatorChar.ToString(), parts);
            expectedPath = Path.Combine(ROOT, OUTPUT, alternatePath);
            if (!File.Exists(expectedPath))
            {
                expectedPath = Path.Combine(ROOT, OUTPUT, path);
            }
#endif
            
            string expected = File.ReadAllText(expectedPath, format.Encoding);
            TypeCobol.Test.TestUtils.compareLines(path, writer.ToString(), expected, PlatformUtils.GetPathForProjectFile(expectedPath, "Codegen\\Test"));

            if (lmStream != null)
            {
                //compare with expected line mapping
                string lm = System.Text.ASCIIEncoding.Default.GetString(lmStream.ToArray());
                string expectedLm = File.ReadAllText(expectedPath + ML_SUFFIX, format.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path + ML_SUFFIX, lm, expectedLm, PlatformUtils.GetPathForProjectFile(expectedPath + ML_SUFFIX, "Codegen\\Test"));
            }
        }

        /// <summary>
        /// Parse and generate using DocumentFormat.RDZReferenceFormat by default, because it's our only real target 
        /// for now and we don't have specifications for FreeFormat.
        /// </summary>
        /// <param name="path"></param>
        /// <param name="typeCobolVersion"></param>
        /// <param name="copies"></param>
        /// <remarks>CpyCopyNameMap option is not supported yet in this method</remarks>
        public static void ParseGenerateCompareWithLineMapping(string path, string typeCobolVersion = null, IList<string> copies = null)
        {
            var options = new TypeCobolOptions() { OptimizeWhitespaceScanning = false };
            ParseGenerateCompare(path, options, DocumentFormat.RDZReferenceFormat, typeCobolVersion, copies, new MemoryStream());
        }

        private static string ML_SUFFIX = ".lm";

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
    }
}

﻿using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
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
        /// <param name="path"></param>
        /// <param name="autoRemarks"></param>
        /// <param name="typeCobolVersion"></param>
        /// <param name="copies"></param>        
        public static void ParseGenerateCompare(string path, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null) {
            ParseGenerateCompare(path, DocumentFormat.RDZReferenceFormat, typeCobolVersion, autoRemarks, copies);
        }
        public static void ParseGenerateCompare(string path, DocumentFormat format, string typeCobolVersion, bool autoRemarks = false, IList<string> copies = null, MemoryStream lmStream = null) {
            var document = Parser.Parse(Path.Combine(ROOT, INPUT, path), format, autoRemarks, copies);
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
            string expected = File.ReadAllText(Path.Combine(ROOT, OUTPUT, path), format.Encoding);
            TypeCobol.Test.TestUtils.compareLines(path, writer.ToString(), expected, PlatformUtils.GetPathForProjectFile(Path.Combine(ROOT, OUTPUT, path), "Codegen\\Test"));

            if (lmStream != null)
            {
                //compare with expected line mapping
                string lm = System.Text.ASCIIEncoding.Default.GetString(lmStream.ToArray());
                string expectedLm = File.ReadAllText(Path.Combine(ROOT, OUTPUT, path + ML_SUFFIX), format.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path + ML_SUFFIX, lm, expectedLm, PlatformUtils.GetPathForProjectFile(Path.Combine(ROOT, OUTPUT, path + ML_SUFFIX), "Codegen\\Test"));
            }
        }

        /// <summary>
        /// Parse and generate using DocumentFormat.RDZReferenceFormat by default, because it's our only real target 
        /// for now and we don't have specifications for FreeFormat.
        /// </summary>
        /// <param name="path"></param>
        /// <param name="autoRemarks"></param>
        /// <param name="typeCobolVersion"></param>
        /// <param name="copies"></param>        
        public static void ParseGenerateCompareWithLineMapping(string path, bool autoRemarks = false, string typeCobolVersion = null, IList<string> copies = null)
        {
            ParseGenerateCompareWithLineMapping(path, DocumentFormat.RDZReferenceFormat, typeCobolVersion, autoRemarks, copies);
        }

        private static string ML_SUFFIX = ".lm";
        public static void ParseGenerateCompareWithLineMapping(string path, DocumentFormat format, string typeCobolVersion, bool autoRemarks = false, IList<string> copies = null)
        {
            ParseGenerateCompare(path, DocumentFormat.RDZReferenceFormat, typeCobolVersion, autoRemarks, copies, new MemoryStream());
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
    }
}

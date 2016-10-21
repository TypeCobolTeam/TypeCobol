using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
// DocumentFormat
using TypeCobol.Tools; // CodeElementDiagnostics


namespace TypeCobol.Codegen.Test {

	[TestClass]
	public class TestCob85Codegen
    {
		[TestMethod]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseCobol85() {
			string file = Path.Combine("Cobol85","FileControl");
            CodegenTestUtils.ParseGenerateCompare(file+".cbl");
		}



		private void ParseGenerateCompare2(string path, List<Skeleton> skeletons) {
			var format = DocumentFormat.RDZReferenceFormat;
            //var document = Parser.Parse(Path.Combine(ROOT, INPUT, path), format);

            string rootDirectory = Path.GetDirectoryName(new DirectoryInfo(path).FullName);
            

            TypeCobolOptions options = new TypeCobolOptions();
            //comparator.paths.sextension = extensions[0].Substring(1);
            CompilationProject project = new CompilationProject("TEST",
                rootDirectory, new[] { "*.cbl", "*.cpy" },
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
		    string filename = Path.GetFileName(path);
            FileCompiler Compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, options, null, false);


		    try
		    {
		        Compiler.CompileOnce();
		    }
		    catch (Exception e)
		    {
                Console.WriteLine(" /!\\ EXCEPTION\n" + e);
            }

            foreach (var diag in Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.ParserDiagnostics)
            {
                Console.WriteLine(diag);
            }

            foreach (var ce in Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements)
            {
                foreach (var ceDiagnostic in ce.Diagnostics)
                {
                    Console.WriteLine(ceDiagnostic);
                }
            }
            foreach (var diag in Compiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.Diagnostics)
            {
                Console.WriteLine(diag);
            }


            var columns = Compiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
			var writer = new StringWriter();
			// write parsing errors
			//WriteErrors(writer, document.Errors[0], "CodeElements", columns);
			//WriteErrors(writer, document.Errors[1], "ProgramClass", columns);
			// write generated code
            /*
			var codegen = new Generator(writer, Compiler.CompilationResultsForProgram.TokensLines, skeletons);
			var program = Compiler.CompilationResultsForProgram.ProgramClassDocumentSnapshot.Program;

			codegen.Generate(program.SyntaxTree.Root, program.SymbolTable, columns);
            */
			// flush
			writer.Close();

			// compare with expected result
			//string expected = File.ReadAllText(Path.Combine(ROOT, OUTPUT, path), format.Encoding);
            //TypeCobol.Test.TestUtils.compareLines(path, writer.ToString(), expected);
        }
		
	}
}

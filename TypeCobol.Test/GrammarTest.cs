using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using System.Diagnostics;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Test {

	[TestClass]
	public class GrammarTest {
		[TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","long")]
		[Ignore] // Ignored, as everybody does not have a Samples folder. Remove this if you do have one.
		public void CheckGrammarCorrectness() {

			int STOP_AFTER_AS_MANY_ERRORS = 1000;
			string regex = "*.PGM";
			string samples = @"Samples";
			string root = PlatformUtils.GetPathForProjectFile(samples);
			string[] files = Directory.GetFiles(root, regex, System.IO.SearchOption.AllDirectories);
			string[] include = { };
			string[] exclude = { };
			bool codegen = false;
			var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;

			System.IO.File.WriteAllText("CheckGrammarResults.txt", "");
			int tested = 0, nbFilesInError = 0, ignores = 0;
			TimeSpan sum = new TimeSpan(0);
			int totalNumberOfErrors = 0;
			foreach (var file in files) {

				string filename = Path.GetFileName(file);
				System.IO.File.AppendAllText("CheckGrammarResults.txt", (filename + ':'));
				bool ignore = include.Length > 0 && !include.Contains(filename);
				if (!ignore) ignore = exclude.Contains(filename);
				if (ignore) {
					ignores++;
					System.IO.File.AppendAllText("CheckGrammarResults.txt", " ignored.\n");
					continue;
				}
				string path = Path.Combine(root, filename);
				Stopwatch watch = new Stopwatch();
				watch.Start();
				var document = Parser.Parse(path, format);
				watch.Stop();
				//TestJSONSerializer.DumpAsJSON(unit.CodeElementsDocumentSnapshot.CodeElements, filename);
				TimeSpan elapsed = watch.Elapsed;
				sum += elapsed;
				string formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
				System.IO.File.AppendAllText("CheckGrammarResults.txt", (" parsed in " + formatted + "\n"));

				tested++;
				bool okay = true;
				if(hasErrors(document.Results.CodeElementsDocumentSnapshot)) {
					okay = false;
					totalNumberOfErrors += checkErrors(filename, document.Results.CodeElementsDocumentSnapshot.ParserDiagnostics);
				}
				if(hasErrors(document.Results.ProgramClassDocumentSnapshot)) {
					okay = false;
					totalNumberOfErrors += checkErrors(filename, document.Results.ProgramClassDocumentSnapshot.Diagnostics);
				}
				if (!okay) {
					nbFilesInError++;
					if (nbFilesInError >= STOP_AFTER_AS_MANY_ERRORS) break;
				}

				if (codegen) {
					var writer = new StringWriter();
					var generator = new TypeCobol.Codegen.Generator(writer, document.Results.TokensLines, null);
					var program = document.Results.ProgramClassDocumentSnapshot.Program;
					var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
					generator.Generate(program.SyntaxTree.Root, program.SymbolTable, columns);
					writer.Close();

					string expected = File.ReadAllText(path);
					Assert.AreEqual(ReplaceLineBreaks(expected), ReplaceLineBreaks(writer.ToString()));
				}
			}
			string total = String.Format("{0:00}m{1:00}s{2:000}ms", sum.Minutes, sum.Seconds, sum.Milliseconds);
			string message = "Files tested=" + tested + "/" + files.Length + ", files in error=" + nbFilesInError + ", ignored=" + ignores + "\nTotal number of errors: "+ totalNumberOfErrors+  "\ntotal time: " + total;
			System.IO.File.AppendAllText("CheckGrammarResults.txt", message);
			if (nbFilesInError > 0) Assert.Fail('\n'+message);
		}

		private bool hasErrors(TypeCobol.Compiler.Parser.CodeElementsDocument document) {
			return document != null && document.ParserDiagnostics != null && document.ParserDiagnostics.Count() > 0;
		}
		private bool hasErrors(TypeCobol.Compiler.Parser.ProgramClassDocument document) {
			return document != null && document.Diagnostics != null && document.Diagnostics.Count() > 0;
		}
		private int checkErrors(string filename, IEnumerable<TypeCobol.Compiler.Diagnostics.Diagnostic> diagnostics) {
			Console.WriteLine(filename);
			string result = ParserUtils.DiagnosticsToString(diagnostics);
			Console.WriteLine(result);
			System.IO.File.AppendAllText("CheckGrammarResults.txt", (result + "\n"));
			return diagnostics.Count();
		}

		private string ReplaceLineBreaks(string text) {
			return text.Replace("\r\n","\n").Replace("\r","\n");
		}
	}
}

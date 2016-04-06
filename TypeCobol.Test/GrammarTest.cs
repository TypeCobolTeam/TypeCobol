using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using System.Diagnostics;
using System.Collections.Generic;

namespace TypeCobol.Test {

	[TestClass]
	public class GrammarTest {
		[TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","long")]
        [Ignore] // Ignored, as everybody does not have a Samples folder. Remove this if you do have one.
        public void CheckGrammarCorrectness()
        {
            int STOP_AFTER_AS_MANY_ERRORS = 1000;
            string regex = "*.PGM";
            string samples = @"Samples";
            string path = PlatformUtils.GetPathForProjectFile(samples);
            string[] files = System.IO.Directory.GetFiles(path, regex, System.IO.SearchOption.AllDirectories);
            string[] include = { };
            string[] exclude = { };
            bool codegen = false;
			var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;

            System.IO.File.WriteAllText("CheckGrammarResults.txt", "");
            int tested = 0, nbFilesInError = 0, ignores = 0;
            TimeSpan sum = new TimeSpan(0);
            int totalNumberOfErrors = 0;
            foreach (var file in files)
            {
                string filename = System.IO.Path.GetFileName(file);
                System.IO.File.AppendAllText("CheckGrammarResults.txt", (filename + ':'));
                bool ignore = include.Length > 0 && !include.Contains(filename);
                if (!ignore) ignore = exclude.Contains(filename);
                if (ignore) {
                    ignores++;
                    System.IO.File.AppendAllText("CheckGrammarResults.txt", " ignored.\n");
                    continue;
                }
                Stopwatch watch = new Stopwatch();
                watch.Start();
                var unit = ParserUtils.ParseCobolFile(filename, format, samples);
                watch.Stop();
                //TestJSONSerializer.DumpAsJSON(unit.CodeElementsDocumentSnapshot.CodeElements, filename);
                TimeSpan elapsed = watch.Elapsed;
                sum += elapsed;
                string formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
                System.IO.File.AppendAllText("CheckGrammarResults.txt", (" parsed in " + formatted + "\n"));

                tested++;
                bool okay = true;
                if(hasErrors(unit.CodeElementsDocumentSnapshot)) {
                    okay = false;
                    totalNumberOfErrors += checkErrors(filename, unit.CodeElementsDocumentSnapshot.ParserDiagnostics);
                }
                if(hasErrors(unit.ProgramClassDocumentSnapshot)) {
                    okay = false;
                    totalNumberOfErrors += checkErrors(filename, unit.ProgramClassDocumentSnapshot.Diagnostics);
                }
                if (!okay) {
                    nbFilesInError++;
                    if (nbFilesInError >= STOP_AFTER_AS_MANY_ERRORS) break;
                }

                if (codegen) {
                    var generator = new TypeCobol.Compiler.Generator.TypeCobolGenerator(null, format, unit.ProgramClassDocumentSnapshot);
					var stream = new System.IO.StreamWriter(filename+".gen");
					generator.WriteCobol(stream);
					stream.Close();
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
	}
}

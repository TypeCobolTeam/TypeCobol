using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using System.Diagnostics;

namespace TypeCobol.Test
{
    [TestClass]
    public class GrammarTest
    {
        [TestMethod]
        [Ignore] // Ignored, as everybody does not have a Samples folder. Remove this if you do have one.
        public void CheckGrammarCorrectness()
        {
            int STOP_AFTER_AS_MANY_ERRORS = 1000;
            string regex = "*.PGM";
            string samples = @"Samples";
            string path = PlatformUtils.GetPathForProjectFile(samples);
            string[] files = System.IO.Directory.GetFiles(path, regex, System.IO.SearchOption.AllDirectories);
            string[] filtered = { };
            string[] ignored = { };

            System.IO.File.WriteAllText("CheckGrammarResults.txt", "");
            int tested = 0, nbFilesInError = 0, ignores = 0;
            TimeSpan sum = new TimeSpan(0);
            int totalNumberOfErrors = 0;
            foreach (var file in files)
            {
                string filename = System.IO.Path.GetFileName(file);
                System.IO.File.AppendAllText("CheckGrammarResults.txt", (filename + ':'));
                bool ignore = filtered.Length > 0 && !filtered.Contains(filename);
                if (!ignore) ignore = ignored.Contains(filename);
                if (ignore) {
                    ignores++;
                    System.IO.File.AppendAllText("CheckGrammarResults.txt", " ignored.\n");
                    continue;
                }
                Stopwatch watch = new Stopwatch();
                watch.Start();
                var unit = ParserUtils.ParseCobolFile(filename, null, samples);
                watch.Stop();
                //TestJSONSerializer.DumpAsJSON(unit.SyntaxDocument.CodeElements, filename);
                TimeSpan elapsed = watch.Elapsed;
                sum += elapsed;
                string formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
                System.IO.File.AppendAllText("CheckGrammarResults.txt", (" parsed in " + formatted + "\n"));

                tested++;
                if(hasErrors(unit.SyntaxDocument)) {
                    Console.WriteLine(filename);
                    string result = ParserUtils.DiagnosticsToString(unit.SyntaxDocument.Diagnostics);
                    totalNumberOfErrors += unit.SyntaxDocument.Diagnostics.Count;
                    Console.WriteLine(result);
                    System.IO.File.AppendAllText("CheckGrammarResults.txt", (result + "\n"));
                    nbFilesInError++;
                    if (nbFilesInError >= STOP_AFTER_AS_MANY_ERRORS) break;
                }
            }
            string total = String.Format("{0:00}m{1:00}s{2:000}ms", sum.Minutes, sum.Seconds, sum.Milliseconds);
            string message = "Files tested=" + tested + "/" + files.Length + ", files in error=" + nbFilesInError + ", ignored=" + ignores + "\nTotal number of errors: "+ totalNumberOfErrors+  "\ntotal time: " + total;
            System.IO.File.AppendAllText("CheckGrammarResults.txt", message);
            if (nbFilesInError > 0) Assert.Fail('\n'+message);
        }

        private bool hasErrors(TypeCobol.Compiler.Parser.SyntaxDocument document)
        {
            return document != null && document.Diagnostics != null && document.Diagnostics.Count > 0;
        }
    }
}
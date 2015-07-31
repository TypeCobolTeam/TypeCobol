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
            int STOP_AFTER_AS_MANY_ERRORS = 1;
            string regex = "*.PGM";
            string samples = @"Samples";
            string path = PlatformUtils.GetPathForProjectFile(samples);
            string[] files = System.IO.Directory.GetFiles(path, regex, System.IO.SearchOption.AllDirectories);
            string[] ignored = { };

            var str = new System.Text.StringBuilder();
            int tested = 0, errors = 0;
            foreach (var file in files)
            {
                string filename = System.IO.Path.GetFileName(file);
                str.Append(filename + ':');
                if (ignored.Contains(filename)) {
                    str.AppendLine(" ignored.");
                    continue;
                }
                Stopwatch watch = new Stopwatch();
                watch.Start();
                var unit = ParserUtils.ParseCobolFile(filename, null, samples);
                watch.Stop();
                TimeSpan elapsed = watch.Elapsed;
                string formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
                str.AppendLine(" parsed in "+formatted);

                tested++;
                if(hasErrors(unit.SyntaxDocument)) {
                    Console.WriteLine(filename);
                    string result = ParserUtils.DumpCodeElements(unit);
                    Console.WriteLine(result);
                    errors++;
                    if (errors >= STOP_AFTER_AS_MANY_ERRORS) break;
                }
            }
            string message = "Files tested=" + tested + "/" + files.Length + ", errors=" + errors;
            System.IO.File.WriteAllText("CheckGrammarResults.txt", str.AppendLine(message).ToString());
            if (errors > 0) Assert.Fail('\n'+message);
        }

        private bool hasErrors(TypeCobol.Compiler.Parser.SyntaxDocument document)
        {
            return document != null && document.Diagnostics != null && document.Diagnostics.Count > 0;
        }
    }
}
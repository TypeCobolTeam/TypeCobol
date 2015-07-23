using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;

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

var format = new TypeCobol.Compiler.DocumentFormat(
    System.Text.Encoding.UTF8,
    TypeCobol.Compiler.File.EndOfLineDelimiter.CrLfCharacters,
    0, TypeCobol.Compiler.Text.ColumnsLayout.FreeTextFormat);

            int tested = 0, errors = 0;
            foreach (var file in files)
            {
                string filename = System.IO.Path.GetFileName(file);
                var unit = ParserUtils.ParseCobolFile(filename, null, samples);
                tested++;
                if(hasErrors(unit.SyntaxDocument)) {
                    Console.WriteLine(filename);
                    string result = ParserUtils.DumpCodeElements(unit);
                    Console.WriteLine(result);
                    errors++;
                    if (errors >= STOP_AFTER_AS_MANY_ERRORS) break;
                }
            }
            string message = "Files tested="+tested+"/"+files.Length+", errors="+errors;
            if (errors > 0) Assert.Fail("\n"+message);
        }

        private bool hasErrors(TypeCobol.Compiler.Parser.SyntaxDocument document)
        {
            return document != null && document.Diagnostics != null && document.Diagnostics.Count > 0;
        }
    }
}
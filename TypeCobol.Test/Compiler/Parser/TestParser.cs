using System;
using System.IO;
using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Parser
{
    static class TestParser
    {
        static string root = PlatformUtils.GetPathForProjectFile("Compiler" + Path.DirectorySeparatorChar + "Parser");
        static string sampleRoot = root + Path.DirectorySeparatorChar + "Samples";
        static string resultRoot = root + Path.DirectorySeparatorChar + "ResultFiles";

        public static void Check_BeforeAfterInsertionBatched()
        {
            Paths paths = new Paths(sampleRoot, resultRoot, sampleRoot + Path.DirectorySeparatorChar + "Programs" + Path.DirectorySeparatorChar + "Simple.pgm", new Multipass.IndexNames());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Init(new[] { ".pgm", ".cpy" });
            unit.Parse();

            var e = updateLine(TextChangeType.LineInserted, 2, "END PROGRAM Simple.");
            e = updateLine(TextChangeType.LineUpdated, 1, "PROGRAM-ID. Simpler.", e);

            // clear document
            unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
            unit.Parse();

            var names = unit.Comparator.paths.Resultnames as Multipass.IndexNames;
            names.index = 2;
            Console.WriteLine("Compare with result file: "+unit.Comparator.paths.Result);
            unit.Compare();//with Simple.2.txt
        }

        public static void Check_BeforeAfterInsertion()
        {
            Paths paths = new Paths(sampleRoot, resultRoot, sampleRoot + Path.DirectorySeparatorChar + "Programs" + Path.DirectorySeparatorChar + "Simple.pgm", new Multipass.IndexNames());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Init(new[] { ".pgm", ".cpy" });
            unit.Parse();
            var names = unit.Comparator.paths.Resultnames as Multipass.IndexNames;
            names.index = 0;
            unit.Compare();//with Simple.0.txt

            // explicitely close program by adding END PROGRAM line
            var e2 = updateLine(TextChangeType.LineInserted, 2, "END PROGRAM Simple.");
            var e = updateLine(TextChangeType.LineUpdated, 1, "  PROGRAM-ID. Simple.");
            e.TextChanges.Add(e2.TextChanges[0]);
            unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
            unit.Parse();
            names.index++;
            Console.WriteLine("Compare with result file: "+unit.Comparator.paths.Result);
            unit.Compare();//with Simple.1.txt

            // change program name ; now first and last line have differing program id
            e = updateLine(TextChangeType.LineUpdated, 1, "PROGRAM-ID. Simpler.");
            unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
            unit.Parse();
            names.index++;
            Console.WriteLine("Compare with result file: "+unit.Comparator.paths.Result);
            unit.Compare();//with Simple.2.txt

            // clear document
            e = updateLine(TextChangeType.DocumentCleared, /* the following parameters are not used when DocumentCleared*/ 0, null);
            unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
            unit.Parse();
            names.index++;
            Console.WriteLine("Compare with result file: "+unit.Comparator.paths.Result);
            unit.Compare();//with Simple.3.txt
        }

        public static void AntlrPerformanceProfiler()
        {
            var AntlrFolder = root + Path.DirectorySeparatorChar + "Antlr";
            Paths paths = new Paths(AntlrFolder, AntlrFolder, AntlrFolder + Path.DirectorySeparatorChar + "AntlrTest.pgm", new AntlrName());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Init(new[] { ".pgm", ".cpy" }, false, true);
            unit.Parse();
          
            unit.Compare(unit.Compiler.CompilationResultsForProgram.AntlrResult);
        }

        private static TextChangedEvent updateLine(TextChangeType type, int line, string text, TextChangedEvent e = null) {
            if (e==null) e = new TextChangedEvent();
            ITextLine snapshot = new TextLineSnapshot(line, text, null);
            e.TextChanges.Add(new TextChange(type, line, snapshot));
            return e;
        }
    }
}

using System;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser.Incremental {

	[TestClass]
	public class TestIncrementalPipeline {

		[TestMethod]
		[TestCategory("Performance")]
		[TestProperty("Time","long")]
		public void CheckPerformance() {
            // Sample program properties
            string folder = "Parser" + Path.DirectorySeparatorChar + "Samples";
            string textName = "BigBatch";
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;

            // Create a FileCompiler for this program
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(folder));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", localDirectory.FullName));
            }
            CompilationProject project = new CompilationProject("test",
                localDirectory.FullName, new string[] { ".cbl", ".cpy" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, new TypeCobolOptions());
            FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions(), null, false, project);
            
            // Execute a first (complete) compilation
            compiler.CompileOnce();

            // Append one line in the middle of the program
            ITextLine newLine = new TextLineSnapshot(9211, "094215D    DISPLAY '-ICLAUA      = ' ICLAUA.                            0000000", null);
            TextChangedEvent textChangedEvent = new TextChangedEvent();
            textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineInserted, 9211, newLine));
            compiler.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
            
            // Execute a second (incremental) compilation
            compiler.CompileOnce();

            // Display a performance report
            StringBuilder report = new StringBuilder();
            report.AppendLine("Program properties :");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.CobolTextLines.Count + " lines");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements.Count() + " code elements");
            report.AppendLine("First compilation performance");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForText.FirstCompilationTime + " ms : text update");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForScanner.FirstCompilationTime + " ms : scanner");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForPreprocessor.FirstCompilationTime + " ms : preprocessor");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.FirstCompilationTime + " ms : code elements parser");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForProgramClassParser.FirstCompilationTime + " ms : program class parser");
            report.AppendLine("Incremental compilation performance");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForText.LastRefreshTime + " ms : text update");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForScanner.LastRefreshTime + " ms : scanner");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForPreprocessor.LastRefreshTime + " ms : preprocessor");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.LastRefreshTime + " ms : code elements parser");
            report.AppendLine("- " + compiler.CompilationResultsForProgram.PerfStatsForProgramClassParser.LastRefreshTime + " ms : program class parser");

            Console.WriteLine(report.ToString());
		}
	}

    [TestClass]
    public class TestChangeEvent
    {
        static readonly string Root = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Programs" + Path.DirectorySeparatorChar + "Cobol85";

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        public void Check_BeforeAfterInsertionBatched()
        {
            Paths paths = new Paths(Root, Root, Root + Path.DirectorySeparatorChar + "Simple.pgm", new Multipass.IndexNames());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Init(new[] { ".pgm", ".cpy" });
            unit.Parse();

            var e = UpdateLine(TextChangeType.LineInserted, 2, "END PROGRAM Simple.");
            e = UpdateLine(TextChangeType.LineUpdated, 1, "PROGRAM-ID. Simpler.", e);

            // clear document
            unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
            unit.Parse();

            var names = unit.Comparator.paths.Resultnames as Multipass.IndexNames;
            if (names != null) names.index = 2;
            Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
            unit.Compare();//with Simple.2.txt
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        public void Check_BeforeAfterInsertion()
        {
            Paths paths = new Paths(Root, Root, Root + Path.DirectorySeparatorChar + "Simple.pgm", new Multipass.IndexNames());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Init(new[] { ".pgm", ".cpy" });
            unit.Parse();
            var names = unit.Comparator.paths.Resultnames as Multipass.IndexNames;
            if (names != null)
            {
                names.index = 0;
                unit.Compare();//with Simple.0.txt

                // explicitely close program by adding END PROGRAM line
                var e2 = UpdateLine(TextChangeType.LineInserted, 2, "END PROGRAM Simple.");
                var e = UpdateLine(TextChangeType.LineUpdated, 1, "  PROGRAM-ID. Simple.");
                e.TextChanges.Add(e2.TextChanges[0]);
                unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
                unit.Parse();
                names.index++;
                Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
                unit.Compare();//with Simple.1.txt

                // change program name ; now first and last line have differing program id
                e = UpdateLine(TextChangeType.LineUpdated, 1, "PROGRAM-ID. Simpler.");
                unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
                unit.Parse();
                names.index++;
                Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
                unit.Compare();//with Simple.2.txt

                // clear document
                e = UpdateLine(TextChangeType.DocumentCleared, /* the following parameters are not used when DocumentCleared*/ 0, null);
                unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
                unit.Parse();
                names.index++;
            }
            Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
            unit.Compare();//with Simple.3.txt
        }


        private static TextChangedEvent UpdateLine(TextChangeType type, int line, string text, TextChangedEvent e = null)
        {
            if (e == null) e = new TextChangedEvent();
            ITextLine snapshot = new TextLineSnapshot(line, text, null);
            e.TextChanges.Add(new TextChange(type, line, snapshot));
            return e;
        }
    }
}

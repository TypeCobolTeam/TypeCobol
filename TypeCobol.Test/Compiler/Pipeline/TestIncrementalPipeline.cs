using System;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;

namespace TypeCobol.Test.Compiler.Pipeline
{
    static class TestIncrementalPipeline
    {
        public static void CheckUpdateFromFile()
        {

        }

        public static void CheckPerformance()
        {
            // Sample program properties
            string folder = "Compiler" + Path.DirectorySeparatorChar + "Pipeline" + Path.DirectorySeparatorChar + "Samples";
            string textName = "BigBatch";
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;

            // Create a FileCompiler for this program
            DirectoryInfo localDirectory = new DirectoryInfo(PlatformUtils.GetPathForProjectFile(folder));
            if (!localDirectory.Exists)
            {
                throw new Exception(String.Format("Directory : {0} does not exist", localDirectory.FullName));
            }
            CompilationProject project = new CompilationProject("test",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout, new TypeCobolOptions());
            FileCompiler compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions(), null, false);
            
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
}

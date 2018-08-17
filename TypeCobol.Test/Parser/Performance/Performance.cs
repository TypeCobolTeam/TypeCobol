using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser.Performance
{
    [TestClass]
    public class Performance
    {
        static readonly string AntlrFolder = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Performance";

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void AntlrPerformanceProfiler()
        {
            Paths paths = new Paths(AntlrFolder, AntlrFolder, AntlrFolder + Path.DirectorySeparatorChar + "AntlrTest.rdz.pgm", new AntlrName());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Init(new[] { ".pgm", ".cpy" }, false, true);
            unit.Parse();

            unit.Compare(unit.Compiler.CompilationResultsForProgram.AntlrResult);
        }


        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "long")]
        public void IncrementalPerformance()
        {
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
            //Make an incremental change to the source code
            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            ExecuteInceremental(compiler, stats);

            // Display a performance report
            TestUtils.CreateRunReport(TestUtils.GetReportDirectoryPath(), compiler.CobolFile.Name + "-Incremental", compiler.CompilationResultsForProgram, stats);
        }

        public static void ExecuteInceremental(FileCompiler compiler, TestUtils.CompilationStats stats)
        {
            // Execute a first (complete) compilation
            compiler.CompileOnce();
            //Iterate multiple times over an incremental change
            int incrementalIterationNumber = 20;
            for (int i = 0; i < incrementalIterationNumber; i++)
            {
                // Append one line in the middle of the program
                ITextLine newLine = new TextLineSnapshot(9211, "094215D    DISPLAY '-ICLAUA      = ' ICLAUA.                            0000000", null);
                TextChangedEvent textChangedEvent = new TextChangedEvent();
                textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineInserted, 9211, newLine));
                compiler.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
                
                // Execute a second (incremental) compilation
                compiler.CompileOnce();
                //Accumulate results
                stats.AverageTextUpdateTime                 += compiler.CompilationResultsForProgram.PerfStatsForText.LastRefreshTime;
                stats.AverageScannerTime                    += compiler.CompilationResultsForProgram.PerfStatsForScanner.LastRefreshTime;
                stats.AveragePreprocessorTime               += compiler.CompilationResultsForProgram.PerfStatsForPreprocessor.LastRefreshTime;
                stats.AverageCodeElementParserTime          += compiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.LastRefreshTime;
                stats.AverateTemporarySemanticsParserTime   += compiler.CompilationResultsForProgram.PerfStatsForTemporarySemantic.LastRefreshTime;
                stats.AverageCrossCheckerParserTime         += compiler.CompilationResultsForProgram.PerfStatsForProgramCrossCheck.LastRefreshTime;
            }
            //Compute average time needed for each phase
            stats.AverageTextUpdateTime                 = (int) stats.AverageTextUpdateTime / incrementalIterationNumber;
            stats.AverageScannerTime                    = (int) stats.AverageScannerTime / incrementalIterationNumber;
            stats.AveragePreprocessorTime               = (int) stats.AveragePreprocessorTime / incrementalIterationNumber;
            stats.AverageCodeElementParserTime          = (int) stats.AverageCodeElementParserTime / incrementalIterationNumber;
            stats.AverateTemporarySemanticsParserTime   = (int) stats.AverateTemporarySemanticsParserTime / incrementalIterationNumber;
            stats.AverageCrossCheckerParserTime         = (int) stats.AverageCrossCheckerParserTime / incrementalIterationNumber;
            stats.AverageTotalProcessingTime = stats.AverageCodeElementParserTime +
                                               stats.AverageCrossCheckerParserTime +
                                               stats.AveragePreprocessorTime +
                                               stats.AverageScannerTime +
                                               stats.AverageTextUpdateTime +
                                               stats.AverateTemporarySemanticsParserTime;
        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void FullParsingAndGenerationTest()
        {
            string[] copiesFolder = new string[] { };
            string pwd = Directory.GetCurrentDirectory();

            var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
            string rootFolder = Directory.GetParent(pwd)?.Parent?.FullName +
                                "\\TypeCobol.Test\\Parser\\Samples";
            string textName = "BigBatch";

            string filename = Path.GetFileName(textName);
            string path = Path.Combine(rootFolder, filename);

            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            int iterationNumber = 20;
            //Warmup before measurement
            var documentWarmup = new TypeCobol.Parser();
            var optionsWarmup = new TypeCobolOptions
            {
                ExecToStep = ExecutionStep.CrossCheck,
#if EUROINFO_RULES
                AutoRemarksEnable = true
#endif
            };
            documentWarmup.Init(path, optionsWarmup, format, copiesFolder);
            documentWarmup.Parse(path);

            for (int i = 0; i < iterationNumber; i++)
            {
                var document = new TypeCobol.Parser();
                var options = new TypeCobolOptions
                {
                    ExecToStep = ExecutionStep.CrossCheck,
#if EUROINFO_RULES
                    AutoRemarksEnable = true
#endif
                };
                document.Init(path, options, format, copiesFolder);
                document.Parse(path);
                stats.AverageTextUpdateTime += document.Results.PerfStatsForText.FirstCompilationTime;
                stats.AverageScannerTime += document.Results.PerfStatsForScanner.FirstCompilationTime;
                stats.AveragePreprocessorTime += document.Results.PerfStatsForPreprocessor.FirstCompilationTime;
                stats.AverageCodeElementParserTime += document.Results.PerfStatsForCodeElementsParser.FirstCompilationTime;
                stats.AverateTemporarySemanticsParserTime +=
                    document.Results.PerfStatsForTemporarySemantic.FirstCompilationTime;
                stats.AverageCrossCheckerParserTime += document.Results.PerfStatsForProgramCrossCheck.FirstCompilationTime;
            }

            //Compute average time needed for each phase
            stats.AverageTextUpdateTime = (int)stats.AverageTextUpdateTime / iterationNumber;
            stats.AverageScannerTime = (int)stats.AverageScannerTime / iterationNumber;
            stats.AveragePreprocessorTime = (int)stats.AveragePreprocessorTime / iterationNumber;
            stats.AverageCodeElementParserTime = (int)stats.AverageCodeElementParserTime / iterationNumber;
            stats.AverateTemporarySemanticsParserTime = (int)stats.AverateTemporarySemanticsParserTime / iterationNumber;
            stats.AverageCrossCheckerParserTime = (int)stats.AverageCrossCheckerParserTime / iterationNumber;

            stats.AverageTotalProcessingTime = stats.AverageCodeElementParserTime +
                                               stats.AverageCrossCheckerParserTime +
                                               stats.AveragePreprocessorTime +
                                               stats.AverageScannerTime +
                                               stats.AverageTextUpdateTime +
                                               stats.AverateTemporarySemanticsParserTime;
            stats.Line = documentWarmup.Results.CobolTextLines.Count;
            stats.TotalCodeElements = documentWarmup.Results.CodeElementsDocumentSnapshot.CodeElements.Count();

            TestUtils.CreateRunReport(TestUtils.GetReportDirectoryPath(), filename + "-FullParsing", null, stats);
        }
    }
}

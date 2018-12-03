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
        [Ignore]
        public void IncrementalPerformance()
        {
            IncrementalPerformance2(PlatformUtils.GetPathForProjectFile("ThirdParty" + Path.DirectorySeparatorChar + "CNAF" + Path.DirectorySeparatorChar + "Batch"), "CGMV01.COB",
                 55987, "           MOVE LOW-VALUE     TO C1M010R.                               CGZPARAK");
        }

        private void IncrementalPerformance2(string folder, string textName, int newLineIndex, string newLineText)
        { 
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;

            // Create a FileCompiler for this program
            DirectoryInfo localDirectory = new DirectoryInfo(folder);
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
            ExecuteInceremental(compiler, stats, newLineIndex, newLineText);

            // Display a performance report
            TestUtils.CreateRunReport("Incremental", TestUtils.GetReportDirectoryPath(), compiler.CobolFile.Name, stats, compiler.CompilationResultsForProgram);
        }

        private static void ExecuteInceremental(FileCompiler compiler, TestUtils.CompilationStats stats, int newLineIndex, string newLineText )
        {
            // Execute a first (complete) compilation
            compiler.CompileOnce();
            //Iterate multiple times over an incremental change
            stats.IterationNumber= 40;
            for (int i = 0; i < stats.IterationNumber; i++)
            {
                // Append one line in the middle of the program
                
                ITextLine newLine = new TextLineSnapshot(newLineIndex, newLineText, null);
                
                TextChangedEvent textChangedEvent = new TextChangedEvent();
                textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineInserted, newLine.LineIndex, newLine));
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
            stats.AverageTextUpdateTime                 = (int) stats.AverageTextUpdateTime / stats.IterationNumber;
            stats.AverageScannerTime                    = (int) stats.AverageScannerTime / stats.IterationNumber;
            stats.AveragePreprocessorTime               = (int) stats.AveragePreprocessorTime / stats.IterationNumber;
            stats.AverageCodeElementParserTime          = (int) stats.AverageCodeElementParserTime / stats.IterationNumber;
            stats.AverateTemporarySemanticsParserTime   = (int) stats.AverateTemporarySemanticsParserTime / stats.IterationNumber;
            stats.AverageCrossCheckerParserTime         = (int) stats.AverageCrossCheckerParserTime / stats.IterationNumber;
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
            string pwd = Directory.GetCurrentDirectory();
            FullParsingAndGenerationTest2(Directory.GetParent(pwd)?.Parent?.FullName + "\\TypeCobol.Test\\ThirdParty\\CNAF\\Batch\\CGMV01.COB");
        }

        private void FullParsingAndGenerationTest2(string path, params string[] copiesFolder)
        { 
            var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;

            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            stats.IterationNumber = 20;
            //Warmup before measurement
            var documentWarmup = new TypeCobol.Parser();
            var optionsWarmup = new TypeCobolOptions
            {
                ExecToStep = ExecutionStep.CrossCheck,
#if EUROINFO_RULES
                AutoRemarksEnable = true
#endif
            };
            

            for (int i = 0; i < stats.IterationNumber; i++)
            {

                documentWarmup = new TypeCobol.Parser();
                documentWarmup.Init(path, optionsWarmup, format, copiesFolder);
                documentWarmup.Parse(path);

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
            stats.AverageTextUpdateTime = (int)stats.AverageTextUpdateTime / stats.IterationNumber;
            stats.AverageScannerTime = (int)stats.AverageScannerTime / stats.IterationNumber;
            stats.AveragePreprocessorTime = (int)stats.AveragePreprocessorTime / stats.IterationNumber;
            stats.AverageCodeElementParserTime = (int)stats.AverageCodeElementParserTime / stats.IterationNumber;
            stats.AverateTemporarySemanticsParserTime = (int)stats.AverateTemporarySemanticsParserTime / stats.IterationNumber;
            stats.AverageCrossCheckerParserTime = (int)stats.AverageCrossCheckerParserTime / stats.IterationNumber;

            stats.AverageTotalProcessingTime = stats.AverageCodeElementParserTime +
                                               stats.AverageCrossCheckerParserTime +
                                               stats.AveragePreprocessorTime +
                                               stats.AverageScannerTime +
                                               stats.AverageTextUpdateTime +
                                               stats.AverateTemporarySemanticsParserTime;
            stats.Line = documentWarmup.Results.CobolTextLines.Count;
            stats.TotalCodeElements = documentWarmup.Results.CodeElementsDocumentSnapshot.CodeElements.Count();


            TestUtils.CreateRunReport("FullParsing", TestUtils.GetReportDirectoryPath(), Path.GetFileNameWithoutExtension(path), stats);
        }
    }
}

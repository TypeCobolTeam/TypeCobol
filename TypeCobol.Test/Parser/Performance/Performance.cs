using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser.Performance
{
    [TestClass]
    [Ignore]
    public class Performance
    {
        static readonly string AntlrFolder = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Performance";

        private static readonly string CNAF_FOLDER = "ThirdParty" + Path.DirectorySeparatorChar + "CNAF" + Path.DirectorySeparatorChar + "Batch" + Path.DirectorySeparatorChar;
        private static readonly string CNAF_TC_FOLDER = "ThirdParty" + Path.DirectorySeparatorChar + "CNAF_TypeCobol" + Path.DirectorySeparatorChar;

        /// <summary>
        /// It's under CNAF_TC_FOLDER because the file was modified for the performance test.
        /// All redefines have been removed.
        ///
        /// It's because others test need to simulate the same code with 
        /// typedef but redefines is forbidden under typedef.
        /// 
        /// With this program we can compare performance pure Cobol85 and a program written with
        /// TypeCobol features(typedef, procedure, global-storage, operator ::, ...)
        /// </summary>
        private static readonly string Cobol85_NoRedefines = CNAF_TC_FOLDER + "CGMV01-Cobol85-NoRedefines.cbl";

        /// <summary>
        /// Almost like Cobol85_NoRedefines file but replace almost level 01 declaration with a typedef.
        /// It's like replacing COPY with typedef
        ///
        /// Performance should be similar to Cobol85_NoRedefines
        /// </summary>
        private static readonly string BigTypes_NoProcedure = CNAF_TC_FOLDER + "CGMV01-BigTypes.tcbl";

        /// <summary>
        /// This file is almost like BigTypes_NoProcedure except all the code is put in a procedure/function instead of the
        /// procedure division of the program.
        ///
        /// The goal is to be sure that no checkers (or any code) make extra works when we are under a procedure/function.
        ///
        /// Performance should be similar to BigTypes_NoProcedure
        /// 
        /// </summary>
        private static readonly string BigTypes_1Procedure = CNAF_TC_FOLDER + "CGMV01-BigTypes-1Procedure.tcbl";

        /// <summary>
        /// Performance should be similar to BigTypes_NoProcedure
        ///
        /// This file is almost like Cobol85_NoRedefines except all variables are declared in Global-storage.
        /// Linkage section is still here
        ///
        /// The goal is to be sure that no checkers (or any code) make extra works when we are under the global-storage
        ///
        /// Performance should be similar to Cobol85_NoRedefines
        /// </summary>
        private static readonly string GlobalStorage = CNAF_TC_FOLDER + "CGMV01-GlobalStorage.tcbl";

        /// <summary>
        /// With TypeCobol, the goal is to have a lot of small procedures that communicates with variable declared in type.
        /// We'll then have the same type use a lot of times in different procedures.
        ///
        /// The goal of this file is to check that there is no performance penalty by declaring a lot of variable with a type
        /// The code doesn't use the variable, we just check the declaration part
        ///
        /// This file only 1 time the type.
        /// It serves as a reference for the other test
        /// </summary>
        private static readonly string UseALotOfTypes_1Times_Reference = CNAF_TC_FOLDER + "CGMV01-UseAlotOfTypes-1Times.tcbl";

        /// <summary>
        /// Same as UseALotOfTypes_1Times_Reference but declare a variable with a type 100 times
        ///
        /// Performance should be similar to UseALotOfTypes_1Times_Reference
        /// </summary>
        private static readonly string UseALotOfTypes_100Times = CNAF_TC_FOLDER + "CGMV01-UseAlotOfTypes-100Times.tcbl";

        /// <summary>
        /// Same as UseALotOfTypes_100Times but the type are used as parameter of a procedure.
        /// 
        /// Performance should be similar to UseALotOfTypes_1Times_Reference
        /// </summary>
        private static readonly string UseALotOfTypes_WithProc_100Times = CNAF_TC_FOLDER + "CGMV01-UseAlotOfTypes-WithProc-100Times.tcbl";

        /// <summary>
        /// Cobol85 with deep variables declaration
        ///
        /// The goal is to see if variable resolution is fast.
        ///
        /// All variables referenced are from the last part (which is the deepest type of DeepTypes TC file).
        /// 
        /// </summary>
        private static readonly string DeepVariables = CNAF_TC_FOLDER + "CGMV01-DeepVariables.cbl";

        /// <summary>
        /// TypeCobol with deep types linked between each other.
        /// This is the TypeCobol version of DeppVariables.
        ///
        /// The goal is to see if type linking, type max depth checking and variable resolution is fast.
        /// All variable resolution are the same that DeepVariables.
        ///
        /// All variables referenced are from the deepest type.
        /// 
        /// </summary>
        private static readonly string DeepTypes = CNAF_TC_FOLDER + "CGMV01-DeepTypes.tcbl";

        /// <summary>
        /// Large Cobol 85 file (more than 200k lines)
        /// </summary>
        private static readonly string LargeFile = CNAF_FOLDER + "CGMX02.COB";

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "fast")]
        //[Ignore]
        public void AntlrPerformanceProfiler()
        {
            var sourceFilePath = Path.Combine(AntlrFolder, "AntlrTest.rdz.pgm");
            var unitTest = new TestUnit(sourceFilePath, antlrProfiling: true);
            var expectedResultPath = Path.Combine(AntlrFolder, "AntlrTest.ANTLR.txt");
            unitTest.AddComparison(Comparisons.GetComparison(expectedResultPath));
            unitTest.Run();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_Cobol85_NoRedefines()
        {
            // Duplicate a MOVE near the end of source
            var rangeUpdate = new RangeUpdate(65808, 80, 65808, 80, Environment.NewLine + "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM       ");
            IncrementalPerformance2(Cobol85_NoRedefines, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_TC_BigTypesNoProcedure()
        {
            // Duplicate a MOVE near the end of source
            var rangeUpdate = new RangeUpdate(65896, 80, 65896, 80, Environment.NewLine + "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM                      CMM010AK");
            IncrementalPerformance2(BigTypes_NoProcedure, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_TC_BigTypesWithProcedure()
        {
            // Duplicate a MOVE near the end of source
            var rangeUpdate = new RangeUpdate(65899, 80, 65899, 80, Environment.NewLine + "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM                      CMM010AK");
            IncrementalPerformance2(BigTypes_1Procedure, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_TC_GlobalStorage()
        {
            // Duplicate a MOVE near the end of source
            var rangeUpdate = new RangeUpdate(65804, 80, 65804, 80, Environment.NewLine + "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM                      CMM010AK");
            IncrementalPerformance2(GlobalStorage, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_001Time()
        {
            // Insert a blank line at the beginning of the file
            var rangeUpdate = new RangeUpdate(50, 0, 50, 0, Environment.NewLine + "                                                                                ");
            IncrementalPerformance2(UseALotOfTypes_1Times_Reference, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_100Times()
        {
            // Insert a blank line at the beginning of the file
            var rangeUpdate = new RangeUpdate(50, 0, 50, 0, Environment.NewLine + "                                                                                ");
            IncrementalPerformance2(UseALotOfTypes_100Times, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_WithProc_100Times()
        {
            // Insert a blank line at the beginning of the file
            var rangeUpdate = new RangeUpdate(50, 0, 50, 0, Environment.NewLine + "                                                                                ");
            IncrementalPerformance2(UseALotOfTypes_WithProc_100Times, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_Incremental_Cobol85_DeepVariables()
        {
            // Insert blank line right before PROCEDURE DIVISION
            var rangeUpdate = new RangeUpdate(20534, 0, 20534, 0, Environment.NewLine + "                                                                                ");
            IncrementalPerformance2(DeepVariables, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_Incremental_TC_DeepTypes()
        {
            // Insert blank line right before PROCEDURE DIVISION
            var rangeUpdate = new RangeUpdate(20692, 0, 20692, 0, Environment.NewLine + "                                                                                ");
            IncrementalPerformance2(DeepTypes, null, rangeUpdate);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part4_Incremental_LargeFile_LineUpdateAtBeginning()
        {
            /*
             * Line 352, we are renaming 88-level 'CEA-TP-RETURN-DUPKEY' to 'CEA-TP-RETURN-DUP-EY'
             * and then readjust the end of the line to match 80 chars.
             */
            var rename = new RangeUpdate(351, 34, 351, 35, "-");
            IncrementalPerformance2(LargeFile, "LineUpdateAtBeginning", rename);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part4_Incremental_LargeFile_LineUpdateAtEnd()
        {
            /*
             * Line 211758, we are changing the sending value of MOVE instruction
             * from '0000' to '1234'.
             */
            var update = new RangeUpdate(211757, 17, 211757, 21, "1234");
            IncrementalPerformance2(LargeFile, "LineUpdateAtEnd", update);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part4_Incremental_LargeFile_LineInsertAtBeginning()
        {
            /*
             * Line 352, we are adding a new 88-level definition 'CEA-TP-RETURN-DUPLABEL'
             * right after 'CEA-TP-RETURN-DUPKEY'
             */
            string text = "              88 CEA-TP-RETURN-DUPLABEL          VALUE +145.            WSHWZ007";
            var insert = new RangeUpdate(351, 80, 351, 80, Environment.NewLine + text);
            IncrementalPerformance2(LargeFile, "LineInsertAtBeginning", insert);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part4_Incremental_LargeFile_LineInsertAtEnd()
        {
            // Line 211758, we are adding a dummy DISPLAY right after the MOVE instruction.
            string text = "           DISPLAY DTOT-PCDRET                                          CMM010AK";
            var insert = new RangeUpdate(211757, 80, 211757, 80, Environment.NewLine + text);
            IncrementalPerformance2(LargeFile, "LineInsertAtEnd", insert);
        }

        /// <summary>
        /// Creates the AnalyzerProvider to be used.
        /// </summary>
        /// <returns></returns>
        protected virtual IAnalyzerProvider CreateAnalyzerProvider()
        {
            return null;
        }

        private void IncrementalPerformance2(string relativePath, string suffix, params RangeUpdate[] updates)
        {
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;
            string fullPath = PlatformUtils.GetPathForProjectFile(relativePath);

            // Create a FileCompiler for this program
            string filename = Path.GetFileName(fullPath);
            var root = new DirectoryInfo(Directory.GetParent(fullPath).FullName);

            CompilationProject project = new CompilationProject("test",
                root.FullName, new[] { ".cbl", ".cpy" },
                documentFormat, new TypeCobolOptions(), CreateAnalyzerProvider());
            FileCompiler compiler = new FileCompiler(null, filename, documentFormat.ColumnsLayout, false, project.SourceFileProvider, project, new TypeCobolOptions(), null, project);
            //Make an incremental change to the source code
            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            ExecuteIncremental(compiler, stats, updates);

            // Display a performance report
            string reportName = "Incremental";
            if (suffix != null) reportName += '_' + suffix;
            TestUtils.CreateRunReport(reportName, TestUtils.GetReportDirectoryPath(), filename, stats, compiler.CompilationResultsForProgram);
        }

        private void ExecuteIncremental(FileCompiler compiler, TestUtils.CompilationStats stats, RangeUpdate[] updates)
        {
            // Execute a first (complete) compilation
            compiler.CompileOnce();

            //Iterate multiple times over an incremental change
            stats.IterationNumber = 40;
            for (int i = 0; i < stats.IterationNumber; i++)
            {
                // Apply range updates
                compiler.CompilationResultsForProgram.UpdateTextLines(updates);

                // Execute a second (incremental) compilation
                compiler.CompileOnce();
                //Be sure that there is no error, otherwise parsing can be incomplete
                var diagnosticCollectionTime = CheckThatThereIsNoError(compiler.CompilationResultsForProgram);

                //Accumulate results
                stats.AverageTextUpdateTime += compiler.CompilationResultsForProgram.PerfStatsForText.LastRefreshTime;
                stats.AverageScannerTime += compiler.CompilationResultsForProgram.PerfStatsForScanner.LastRefreshTime;
                stats.AveragePreprocessorTime += compiler.CompilationResultsForProgram.PerfStatsForPreprocessor.LastRefreshTime;
                stats.AverageCodeElementParserTime += compiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.LastRefreshTime;
                stats.AverateTemporarySemanticsParserTime += compiler.CompilationResultsForProgram.PerfStatsForTemporarySemantic.LastRefreshTime;
                stats.AverageCrossCheckerParserTime += compiler.CompilationResultsForProgram.PerfStatsForProgramCrossCheck.LastRefreshTime;
                stats.AverageQualityCheckerParserTime += compiler.CompilationResultsForProgram.PerfStatsForCodeQualityCheck.LastRefreshTime;
                stats.AverageDiagnosticCollectionTime += (float)diagnosticCollectionTime.TotalMilliseconds;
            }
            //Compute average time needed for each phase
            stats.AverageTextUpdateTime = (int)stats.AverageTextUpdateTime / stats.IterationNumber;
            stats.AverageScannerTime = (int)stats.AverageScannerTime / stats.IterationNumber;
            stats.AveragePreprocessorTime = (int)stats.AveragePreprocessorTime / stats.IterationNumber;
            stats.AverageCodeElementParserTime = (int)stats.AverageCodeElementParserTime / stats.IterationNumber;
            stats.AverateTemporarySemanticsParserTime = (int)stats.AverateTemporarySemanticsParserTime / stats.IterationNumber;
            stats.AverageCrossCheckerParserTime = (int)stats.AverageCrossCheckerParserTime / stats.IterationNumber;
            stats.AverageQualityCheckerParserTime = (int)stats.AverageQualityCheckerParserTime / stats.IterationNumber;
            stats.AverageDiagnosticCollectionTime = (int)stats.AverageDiagnosticCollectionTime / stats.IterationNumber;
            stats.AverageTotalProcessingTime = stats.AverageTextUpdateTime +
                                               stats.AverageScannerTime +
                                               stats.AveragePreprocessorTime +
                                               stats.AverageCodeElementParserTime +
                                               stats.AverateTemporarySemanticsParserTime +
                                               stats.AverageCrossCheckerParserTime +
                                               stats.AverageQualityCheckerParserTime +
                                               stats.AverageDiagnosticCollectionTime;
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_Cobol85_NoRedefines()
        {
            FullParsing(Cobol85_NoRedefines);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_TC_BigTypesNoProcedure()
        {
            FullParsing(BigTypes_NoProcedure);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_TC_BigTypesWithProcedure()
        {
            FullParsing(BigTypes_1Procedure);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_TC_GlobalStorage()
        {
            FullParsing(GlobalStorage);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_001Time()
        {
            FullParsing(UseALotOfTypes_1Times_Reference);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_100Times()
        {
            FullParsing(UseALotOfTypes_100Times);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_WithProc_100Times()
        {
            FullParsing(UseALotOfTypes_WithProc_100Times);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_FullParsing_Cobol85_DeepVariables()
        {
            FullParsing(DeepVariables);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_FullParsing_TC_DeepTypes()
        {
            FullParsing(DeepTypes);
        }

        /// <summary>
        /// Method for parsinga document.
        /// </summary>
        /// <param name="fullPath"></param>
        /// <param name="options"></param>
        /// <param name="format"></param>
        /// <param name="copiesFolder"></param>
        /// <returns></returns>
        protected virtual TypeCobol.Parser ParseDocument(string fullPath, TypeCobolOptions options, TypeCobol.Compiler.DocumentFormat format, string[] copiesFolder)
        {
            var document = new TypeCobol.Parser();
            document.Init(fullPath, false, options, format, copiesFolder, CreateAnalyzerProvider());
            document.Parse(fullPath);
            return document;
        }

        private void FullParsing(string relativePath, params string[] copiesFolder)
        {
            string fullPath = PlatformUtils.GetPathForProjectFile(relativePath);
            var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;

            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            stats.IterationNumber = 20;

            //Warmup before measurement
            var options = new TypeCobolOptions { ExecToStep = ExecutionStep.CrossCheck };
            var documentWarmup = ParseDocument(fullPath, options, format, copiesFolder);
            //Be sure that there is no error, otherwise parsing can be incomplete
            CheckThatThereIsNoError(documentWarmup.Results); //Discard diagnostics collection time for this warmup run

            for (int i = 0; i < stats.IterationNumber; i++)
            {
                var document = ParseDocument(fullPath, options, format, copiesFolder);
                AllDiagnostics(document.Results, out var diagnosticCollectionTime);

                stats.AverageTextUpdateTime += document.Results.PerfStatsForText.FirstCompilationTime;
                stats.AverageScannerTime += document.Results.PerfStatsForScanner.FirstCompilationTime;
                stats.AveragePreprocessorTime += document.Results.PerfStatsForPreprocessor.FirstCompilationTime;
                stats.AverageCodeElementParserTime += document.Results.PerfStatsForCodeElementsParser.FirstCompilationTime;
                stats.AverateTemporarySemanticsParserTime += document.Results.PerfStatsForTemporarySemantic.FirstCompilationTime;
                stats.AverageCrossCheckerParserTime += document.Results.PerfStatsForProgramCrossCheck.FirstCompilationTime;
                stats.AverageQualityCheckerParserTime += document.Results.PerfStatsForCodeQualityCheck.FirstCompilationTime;
                stats.AverageDiagnosticCollectionTime += (float)diagnosticCollectionTime.TotalMilliseconds;
            }

            //Compute average time needed for each phase
            stats.AverageTextUpdateTime = (int)stats.AverageTextUpdateTime / stats.IterationNumber;
            stats.AverageScannerTime = (int)stats.AverageScannerTime / stats.IterationNumber;
            stats.AveragePreprocessorTime = (int)stats.AveragePreprocessorTime / stats.IterationNumber;
            stats.AverageCodeElementParserTime = (int)stats.AverageCodeElementParserTime / stats.IterationNumber;
            stats.AverateTemporarySemanticsParserTime = (int)stats.AverateTemporarySemanticsParserTime / stats.IterationNumber;
            stats.AverageCrossCheckerParserTime = (int)stats.AverageCrossCheckerParserTime / stats.IterationNumber;
            stats.AverageQualityCheckerParserTime = (int)stats.AverageQualityCheckerParserTime / stats.IterationNumber;
            stats.AverageDiagnosticCollectionTime = (int)stats.AverageDiagnosticCollectionTime / stats.IterationNumber;
            stats.AverageTotalProcessingTime = stats.AverageTextUpdateTime +
                                               stats.AverageScannerTime +
                                               stats.AveragePreprocessorTime +
                                               stats.AverageCodeElementParserTime +
                                               stats.AverateTemporarySemanticsParserTime +
                                               stats.AverageCrossCheckerParserTime +
                                               stats.AverageQualityCheckerParserTime +
                                               stats.AverageDiagnosticCollectionTime;
            stats.Line = documentWarmup.Results.CobolTextLines.Count;
            stats.TotalCodeElements = documentWarmup.Results.CodeElementsDocumentSnapshot.CodeElements.Count();


            TestUtils.CreateRunReport("FullParsing", TestUtils.GetReportDirectoryPath(), Path.GetFileNameWithoutExtension(fullPath), stats);
        }

        /// <summary>
        ///Be sure that there is no error, otherwise parsing can be incomplete
        ///
        ///We want to be sure that all steps of the parsing are done correctly, otherwise performance
        ///cannot be compared
        /// </summary>
        private TimeSpan CheckThatThereIsNoError(CompilationUnit compilationUnit)
        {
            var diags = AllDiagnostics(compilationUnit, out var duration);
            if (diags.Any(d => d.Info.Severity == Severity.Error))
            {
                throw new Exception("Error diagnostics Detected");
            }

            return duration;
        }

        private static IList<Diagnostic> AllDiagnostics(CompilationUnit compilationUnit, out TimeSpan duration)
        {
            var start = DateTime.Now;
            var result = compilationUnit.AllDiagnostics();
            duration = DateTime.Now - start;
            return result;
        }
    }
}

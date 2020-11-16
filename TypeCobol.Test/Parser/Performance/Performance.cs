﻿using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
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


        private static readonly string CNAF_FOLDER = "TypeCobol.Test" + Path.DirectorySeparatorChar + "ThirdParty" + Path.DirectorySeparatorChar + "CNAF" + Path.DirectorySeparatorChar + "Batch" + Path.DirectorySeparatorChar;
        private static readonly string CNAF_TC_FOLDER = "TypeCobol.Test" + Path.DirectorySeparatorChar + "ThirdParty" + Path.DirectorySeparatorChar + "CNAF_TypeCobol" + Path.DirectorySeparatorChar;


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
        private static readonly string GlobalStorage = CNAF_TC_FOLDER + "CGMV01-GlobalStoragey.tcbl";

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
        /// All variable resolution are the same that DeppVariables.
        ///
        /// All variables referenced are from the deepest type.
        /// 
        /// </summary>
        private static readonly string DeepTypes = CNAF_TC_FOLDER + "CGMV01-DeepTypes.tcbl";


        




        [TestMethod]
        [TestCategory("Performance")]
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
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_Incremental_Cobol85_NoRedefines()
        {
            IncrementalPerformance2(Cobol85_NoRedefines, 65809, "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM       ");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_Incremental_TC_BigTypesNoProcedure()
        {
            IncrementalPerformance2(BigTypes_NoProcedure, 65897, "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM                      CMM010AK");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_Incremental_TC_BigTypesWithProcedure()
        {
            IncrementalPerformance2(BigTypes_1Procedure,65899, "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM                      CMM010AK");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_Incremental_TC_GlobaStorage()
        {
            IncrementalPerformance2(BigTypes_1Procedure,65807, "           MOVE WS-CMM010-MOIS-BIN TO WS-CMM010-MM                      CMM010AK");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_001Time()
        {
            IncrementalPerformance2(UseALotOfTypes_1Times_Reference,50, "                                                                                ");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_100Times()
        {
            IncrementalPerformance2(UseALotOfTypes_100Times,50, "                                                                                ");
        }
        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_WithProc_100Times()
        {
            IncrementalPerformance2(UseALotOfTypes_WithProc_100Times, 50, "                                                                                ");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part3_Incremental_Cobol85_DeepVariables()
        {
            IncrementalPerformance2(DeepVariables,20535, "                                                                                ");
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part3_Incremental_TC_DeepTypes()
        {
            IncrementalPerformance2(DeepTypes,20692, "                                                                                ");
        }

        private void IncrementalPerformance2(string relativePath, int newLineIndex, string newLineText)
        { 
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;
            string fullPath = Directory.GetParent(Directory.GetCurrentDirectory())?.Parent?.FullName + "\\" + relativePath;

            // Create a FileCompiler for this program
            string filename = Path.GetFileName(fullPath);
            var root = new DirectoryInfo(Directory.GetParent(fullPath).FullName);

            CompilationProject project = new CompilationProject("test",
                root.FullName, new[] { ".cbl", ".cpy" },
                documentFormat, new TypeCobolOptions(), null);
            FileCompiler compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, documentFormat.ColumnsLayout, new TypeCobolOptions(), null, false, project);
            //Make an incremental change to the source code
            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            ExecuteIncremental(compiler, stats, newLineIndex, newLineText);

            // Display a performance report
            TestUtils.CreateRunReport("Incremental", TestUtils.GetReportDirectoryPath(), compiler.CobolFile.Name, stats, compiler.CompilationResultsForProgram);
        }

        private void ExecuteIncremental(FileCompiler compiler, TestUtils.CompilationStats stats, int newLineIndex, string newLineText )
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
                //Be sure that there is no error, otherwise parsing can be incomplete
                CheckThatThereIsNoError(compiler.CompilationResultsForProgram);

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
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_FullParsing_Cobol85_NoRedefines()
        {
            FullParsing(Cobol85_NoRedefines);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_FullParsing_TC_BigTypesNoProcedure()
        {
            FullParsing( BigTypes_NoProcedure);
        }
        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_FullParsing_TC_BigTypesWithProcedure()
        {
            FullParsing( BigTypes_1Procedure);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part1_FullParsing_TC_GlobalStorage()
        {
            FullParsing(GlobalStorage);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_001Time()
        {
            FullParsing(UseALotOfTypes_1Times_Reference);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_100Times()
        {
            FullParsing(UseALotOfTypes_100Times);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_WithProc_100Times()
        {
            FullParsing(UseALotOfTypes_WithProc_100Times);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part3_FullParsing_Cobol85_DeepVariables()
        {
            FullParsing(DeepVariables);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        [Ignore]
        public void Part3_FullParsing_TC_DeepTypes()
        {
            FullParsing(DeepTypes);
        }







        private void FullParsing(string relativePath, params string[] copiesFolder)
        {
            string fullPath = Directory.GetParent(Directory.GetCurrentDirectory())?.Parent?.FullName + "\\" + relativePath;
            var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;

            TestUtils.CompilationStats stats = new TestUtils.CompilationStats();
            stats.IterationNumber = 20;
            //Warmup before measurement
            var documentWarmup = new TypeCobol.Parser();
            var options = new TypeCobolOptions
            {
                ExecToStep = ExecutionStep.CrossCheck,
#if EUROINFO_RULES
                AutoRemarksEnable = true
#endif
            };


            //Warmup
            documentWarmup = new TypeCobol.Parser();
            documentWarmup.Init(fullPath, options, format, copiesFolder);
            documentWarmup.Parse(fullPath);
            //Be sure that there is no error, otherwise parsing can be incomplete
            CheckThatThereIsNoError(documentWarmup.Results);

            for (int i = 0; i < stats.IterationNumber; i++)
            {
                var document = new TypeCobol.Parser();
                document.Init(fullPath, options, format, copiesFolder);
                document.Parse(fullPath);

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


            TestUtils.CreateRunReport("FullParsing", TestUtils.GetReportDirectoryPath(), Path.GetFileNameWithoutExtension(fullPath), stats);
        }

        /// <summary>
        ///Be sure that there is no error, otherwise parsing can be incomplete
        ///
        ///We want to be sure that all steps of the parsing are done correctly, otherwise performance
        ///cannot be compared
        /// </summary>
        private void CheckThatThereIsNoError(CompilationUnit compilationUnit)
        {
            if (compilationUnit.AllDiagnostics().Any(d => d.Info.Severity == Severity.Error))
            {
                throw new Exception("Error diagnostics Detected");
            }
        }
    }
}

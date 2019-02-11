using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Antlr4.Runtime.Misc;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;

namespace TypeCobol.Test
{
    public class TestUtils
    {

        //folder name for test results
        private static string _report = "PerformanceReports";

        /// <summary>
        /// Compare result and expectedResult line by line.
        /// If there is at least one difference, throw an exception for the test named by the parameter testName or 
        /// Replace ExpectedResult content if content is different and boolean "autoReplace" is true
        /// </summary>
        /// <param name="testName">Name of the test</param>
        /// <param name="result"></param>
        /// <param name="expectedResult"></param>
        /// <param name="expectedResultPath"></param>
        /// <returns></returns>
        public static void compareLines(string testName, string result, string expectedResult, string expectedResultPath)
        {
            StringBuilder errors = new StringBuilder();

            //Set to true to automaticaly replace content in ExpectedResult File
            bool autoReplace = false;

            if (testName == string.Empty && result == string.Empty && expectedResult == string.Empty &&
                expectedResultPath == string.Empty)
            {
                if (autoReplace)
                    Assert.Fail("Set AutoReplace to false in TestUtils.compareLines()\n\n");
            }

            result = Regex.Replace(result, "(?<!\r)\n", "\r\n");
            expectedResult = Regex.Replace(expectedResult, "(?<!\r)\n", "\r\n");

            String[] expectedResultLines = expectedResult.Split('\r', '\n');
            String[] resultLines = result.Split('\r', '\n');

            var linefaults = new List<int>();
            for (int c = 0; c < resultLines.Length && c < expectedResultLines.Length; c++)
            {
                if (expectedResultLines[c] != resultLines[c]) linefaults.Add(c / 2 + 1);
            }

            if (result != expectedResult)
            {
                if (autoReplace && expectedResultPath != null)
                {
                    replaceLines(result, expectedResultPath);
                    errors.AppendLine("result != expectedResult  In test:" + testName);
                    errors.AppendLine("at line" + (linefaults.Count > 1 ? "s" : "") + ": " + string.Join(",", linefaults));
                    errors.AppendLine("Output file has been modified\n");
                    errors.AppendLine("Please rerun unit test\n");
                }
                else
                {
                    errors.Append("result != expectedResult  In test:" + testName)
                        .AppendLine(" at line" + (linefaults.Count > 1 ? "s" : "") + ": " + string.Join(",", linefaults));
                    errors.AppendLine("See TestUtils.cs compareLines method to autoreplace ExpectedResult");
                    errors.Append("=== RESULT ==========\n" + result + "====================");
                }
                throw new Exception(errors.ToString());

            }
        }

        private static void replaceLines(string result, string expectedResultPath)
        {
            using (StreamWriter writer = new StreamWriter(expectedResultPath))
            {
                writer.Write(result);
            }

        }

        public static string GetReportDirectoryPath()
        {
            return Path.Combine(Directory.GetCurrentDirectory(), _report);
        }

        public static void CreateRunReport(string reportName, string localDirectoryFullName, string cobolFileName,
            [NotNull] CompilationStats stats, CompilationUnit compiler = null)
        {

            // Display a performance report
            StringBuilder report = new StringBuilder();
            report.AppendLine("Program properties :");

            report.AppendLine("- " + (compiler?.CobolTextLines.Count ?? stats.Line) + " lines");
            report.AppendLine("- " + (compiler?.CodeElementsDocumentSnapshot.CodeElements.Count() ?? stats.TotalCodeElements) + " code elements");

            report.AppendLine(" Iteration : " + stats.IterationNumber); 

            if (compiler != null)
            {
                var totalTime = compiler.PerfStatsForText.FirstCompilationTime +
                                compiler.PerfStatsForScanner.FirstCompilationTime +
                                compiler.PerfStatsForPreprocessor.FirstCompilationTime +
                                compiler.PerfStatsForCodeElementsParser.FirstCompilationTime +
                                compiler.PerfStatsForTemporarySemantic.FirstCompilationTime +
                                compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime;

                report.AppendLine("");
                report.AppendLine("First compilation performance");
                report.AppendLine("- " + compiler.PerfStatsForText.FirstCompilationTime + " ms" +
                                  FormatPrecentage(compiler.PerfStatsForText.FirstCompilationTime, totalTime) +
                                  " : text update");
                report.AppendLine("- " + compiler.PerfStatsForScanner.FirstCompilationTime + " ms" +
                                  FormatPrecentage(compiler.PerfStatsForScanner.FirstCompilationTime, totalTime) +
                                  " : scanner");
                report.AppendLine("- " + compiler.PerfStatsForPreprocessor.FirstCompilationTime + " ms" +
                                  FormatPrecentage(compiler.PerfStatsForPreprocessor.FirstCompilationTime, totalTime) +
                                  " : preprocessor");
                report.AppendLine("- " + compiler.PerfStatsForCodeElementsParser.FirstCompilationTime + " ms" +
                                  FormatPrecentage(compiler.PerfStatsForCodeElementsParser.FirstCompilationTime,
                                      totalTime) + " : code elements parser");
                report.AppendLine("- " + compiler.PerfStatsForTemporarySemantic.FirstCompilationTime + " ms" +
                                  FormatPrecentage(compiler.PerfStatsForTemporarySemantic.FirstCompilationTime,
                                      totalTime) + " : temporary semantic class parser");
                report.AppendLine("- " + compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime + " ms" +
                                  FormatPrecentage(compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime,
                                      totalTime) + " : cross check class parser");
                report.AppendLine("TAT " + totalTime + " - ms");
                report.AppendLine("*TAT - Total average time");
            }

            report.AppendLine("");
            report.AppendLine(compiler != null
                ? "Incremental compilation performance (average time)"
                : "Full compilation performance (average time)");
            report.AppendLine("- " + stats.AverageTextUpdateTime + " ms " +
                              FormatPrecentage(stats.AverageTextUpdateTime, stats.AverageTotalProcessingTime) +
                              " : text update");
            report.AppendLine("- " + stats.AverageScannerTime + " ms " +
                              FormatPrecentage(stats.AverageScannerTime, stats.AverageTotalProcessingTime) +
                              " : scanner");
            report.AppendLine("- " + stats.AveragePreprocessorTime + " ms" +
                              FormatPrecentage(stats.AveragePreprocessorTime, stats.AverageTotalProcessingTime) +
                              " : preprocessor");
            report.AppendLine("- " + stats.AverageCodeElementParserTime + " ms" +
                              FormatPrecentage(stats.AverageCodeElementParserTime,
                                  stats.AverageTotalProcessingTime) + " : code elements parser");
            report.AppendLine("- " + stats.AverateTemporarySemanticsParserTime + " ms " +
                              FormatPrecentage(stats.AverateTemporarySemanticsParserTime,
                                  stats.AverageTotalProcessingTime) + " : temporary semantic class parser");
            report.AppendLine("- " + stats.AverageCrossCheckerParserTime + " ms " +
                              FormatPrecentage(stats.AverageCrossCheckerParserTime,
                                  stats.AverageTotalProcessingTime) + " : cross check class parser");
            report.AppendLine("TAT " + stats.AverageTotalProcessingTime + " - ms");
            report.AppendLine("*TAT - Total average time");

            var reportFile = reportName + "_" + cobolFileName.Split('.')[0] + "_" +
                                DateTime.Now.ToString("yyyMMdd_HH_mm_ss") + ".txt";
            Directory.CreateDirectory(GetReportDirectoryPath());
            File.WriteAllText(Path.Combine(localDirectoryFullName, reportFile), report.ToString());
            Console.WriteLine(report.ToString());
        }

        private static string FormatPrecentage(float averageTime, float totalTime)
        {
            return " ( " + (averageTime * 100 / totalTime).ToString("##.##") + " % of TAT)";
        }

        private static string FormatPrecentage(int averageTime, int totalTime)
        {
            return " ( " + (averageTime * 100 / (float)totalTime).ToString("##.##") + " % of TAT)";
        }


        public class CompilationStats
        {
            public CompilationStats()
            {
                IterationNumber = 0;
                AverageCodeElementParserTime = 0;
                AverageCrossCheckerParserTime = 0;
                AveragePreprocessorTime = 0;
                AverageScannerTime = 0;
                AverageTextUpdateTime = 0;
                AverateTemporarySemanticsParserTime = 0;
                AverageTotalProcessingTime = 0;
                Line = 0;
                TotalCodeElements = 0;
            }
            public float IterationNumber { get; set; }
            public float AverageTextUpdateTime { get; set; }
            public float AverageScannerTime { get; set; }
            public float AveragePreprocessorTime { get; set; }
            public float AverageCodeElementParserTime { get; set; }
            public float AverateTemporarySemanticsParserTime { get; set; }
            public float AverageCrossCheckerParserTime { get; set; }
            public float AverageTotalProcessingTime { get; set; }
            //Number of lines in Cobol file
            public int Line { get; set; }
            //Number of CodeElements found during the parsing
            public int TotalCodeElements { get; set; }
        }
    }
}

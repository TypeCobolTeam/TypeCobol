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
                FormatLine(compiler.PerfStatsForText.FirstCompilationTime, totalTime, "text update");
                FormatLine(compiler.PerfStatsForScanner.FirstCompilationTime, totalTime, "scanner");
                FormatLine(compiler.PerfStatsForPreprocessor.FirstCompilationTime, totalTime, "preprocessor");
                FormatLine(compiler.PerfStatsForCodeElementsParser.FirstCompilationTime, totalTime, "code elements parser");
                FormatLine(compiler.PerfStatsForTemporarySemantic.FirstCompilationTime, totalTime, "temporary semantic class parser");
                FormatLine(compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime, totalTime, "cross check class parser");

                report.AppendLine("Total: " + totalTime.ToString("##0.00") + " ms");
            }

            report.AppendLine("");
            report.AppendLine(compiler != null
                ? "Incremental compilation performance (average time)"
                : "Full compilation performance (average time)");

            FormatLine(stats.AverageTextUpdateTime, stats.AverageTotalProcessingTime, "text update");
            FormatLine(stats.AverageScannerTime, stats.AverageTotalProcessingTime, "scanner");
            FormatLine(stats.AveragePreprocessorTime, stats.AverageTotalProcessingTime, "preprocessor");
            FormatLine(stats.AverageCodeElementParserTime, stats.AverageTotalProcessingTime, "code elements parser");
            FormatLine(stats.AverateTemporarySemanticsParserTime, stats.AverageTotalProcessingTime, "temporary semantic class parser");
            FormatLine(stats.AverageCrossCheckerParserTime, stats.AverageTotalProcessingTime, "cross check class parser");

            report.AppendLine("Total average time: " + stats.AverageTotalProcessingTime.ToString("##0.00") + " ms");

            var reportFile = reportName + "_" + cobolFileName.Split('.')[0] + "_" +
                                DateTime.Now.ToString("yyyMMdd_HH_mm_ss") + ".txt";
            Directory.CreateDirectory(GetReportDirectoryPath());
            File.WriteAllText(Path.Combine(localDirectoryFullName, reportFile), report.ToString());
            Console.WriteLine(report.ToString());


            void FormatLine(float time, float totalTime, string text)
            {
                report.AppendLine($"{time,10:#####0.00} ms " + FormatPercentage(time, totalTime) + " " + text);
            }
        }

        

        private static string FormatPercentage(float averageTime, float totalTime)
        {
            return $" ({(averageTime * 100 / totalTime),5:#0.00} %)";
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

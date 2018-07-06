using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using TypeCobol.Compiler;

namespace TypeCobol.Test
{
    public class TestUtils
    {

        //folder name for test results
        private static string _report = "PerformanceReports";

        /// <summary>
        /// Compare result and expectedResult line by line.
        /// If there is at least one difference, throw an exception for the test named by the parameter testName
        /// </summary>
        /// <param name="testName">Name of the test</param>
        /// <param name="result"></param>
        /// <param name="expectedResult"></param>
        /// <returns></returns>
        public static void compareLines(string testName, string result, string expectedResult)
        {
            StringBuilder errors = new StringBuilder();

            result = Regex.Replace(result, "(?<!\r)\n", "\r\n");
            expectedResult = Regex.Replace(expectedResult, "(?<!\r)\n", "\r\n");

            String[] expectedResultLines = expectedResult.Split('\r', '\n' );
            String[] resultLines = result.Split('\r', '\n');

            var linefaults = new List<int>();
            for (int c = 0; c < resultLines.Length && c < expectedResultLines.Length; c++) {
                if (expectedResultLines[c] != resultLines[c]) linefaults.Add(c/2+1);
            }

            if (result != expectedResult)
            {
                errors.Append("result != expectedResult  In test:" + testName)
                      .AppendLine(" at line"+(linefaults.Count>1?"s":"")+": "+string.Join(",", linefaults));
                errors.Append("=== RESULT ==========\n" + result + "====================");
                throw new Exception(errors.ToString());
            }
        }

        public static string GetReportDirectoryPath()
        {
            string pwd = Directory.GetCurrentDirectory();
            return Path.Combine(Directory.GetParent(pwd)?.FullName, _report);
        }

        public static void CreateRunReport(string localDirectoryFullName, string cobolFileName,
            CompilationUnit compiler = null, CompilationStats stats = null)
        {

            // Display a performance report
            StringBuilder report = new StringBuilder();
            report.AppendLine("Program properties :");

            report.AppendLine("- " + (compiler?.CobolTextLines.Count ?? stats?.Line) + " lines");
            report.AppendLine("- " + (compiler?.CodeElementsDocumentSnapshot.CodeElements.Count() ?? stats?.TotalCodeElements) + " code elements");
            if (compiler != null)
            {
                report.AppendLine("First compilation performance");
                report.AppendLine("- " + compiler.PerfStatsForText.FirstCompilationTime + " ms : text update");
                report.AppendLine("- " + compiler.PerfStatsForScanner.FirstCompilationTime + " ms : scanner");
                report.AppendLine("- " + compiler.PerfStatsForPreprocessor.FirstCompilationTime + " ms : preprocessor");
                report.AppendLine("- " + compiler.PerfStatsForCodeElementsParser.FirstCompilationTime +
                                  " ms : code elements parser");
                report.AppendLine("- " + compiler.PerfStatsForTemporarySemantic.FirstCompilationTime +
                                  " ms : temporary semantic class parser");
                report.AppendLine("- " + compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime +
                                  " ms : cross check class parser");
            }

            if (stats != null)
            {
                report.AppendLine(compiler != null
                    ? "Incremental compilation performance (average time)"
                    : "Full compilation performance (average time)");
                report.AppendLine("- " + stats.AverageTextUpdateTime + " ms : text update");
                report.AppendLine("- " + stats.AverageScannerTime + " ms : scanner");
                report.AppendLine("- " + stats.AveragePreprocessorTime + " ms : preprocessor");
                report.AppendLine("- " + stats.AverageCodeElementParserTime + " ms : code elements parser");
                report.AppendLine("- " + stats.AverateTemporarySemanticsParserTime +
                                  " ms : temporary semantic class parser");
                report.AppendLine("- " + stats.AverageCrossCheckerParserTime + " ms : cross check class parser");
            }

            string reportFile = "Report_" + cobolFileName.Split('.')[0] + "_" +
                                DateTime.Now.ToString().Replace("/", "_").Replace(":", "_").Replace(" ", "_") + "_" +
                                DateTime.Now.Millisecond + ".txt";
            Directory.CreateDirectory(GetReportDirectoryPath());
            File.WriteAllText(Path.Combine(localDirectoryFullName, reportFile), report.ToString());
            Console.WriteLine(report.ToString());
        }



        public class CompilationStats
        {
            public CompilationStats()
            {
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

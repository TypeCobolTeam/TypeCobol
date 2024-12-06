using System.Text;
using System.Text.RegularExpressions;
using Antlr4.Runtime.Misc;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;

namespace TypeCobol.Test
{
    public class TestUtils
    {
        public class FileInfo
        {
            public string Path { get; }

            public Encoding Encoding { get; }

            public FileInfo(string path, Encoding encoding = null)
            {
                Path = path;
                Encoding = encoding ?? Encoding.UTF8;
            }

            public string ReadAllText() => File.ReadAllText(Path, Encoding);

            public string[] ReadAllLines() => SplitLines(ReadAllText());

            public void WriteAllLines(string[] contents)
            {
                using (var writer = new StreamWriter(new FileStream(Path, FileMode.Truncate), Encoding))
                {
                    for (int i = 0; i < contents.Length; i++)
                    {
                        string line = contents[i];
                        if (i == contents.Length - 1)
                        {
                            writer.Write(line);
                        }
                        else
                        {
                            writer.WriteLine(line);
                        }
                    }
                }
            }
        }

        private static string[] SplitLines(string content) => content.ReplaceLineEndings().Split(Environment.NewLine);

        public static void CompareFiles(string testName, FileInfo actual, FileInfo expected)
        {
            CompareContent(testName, actual?.ReadAllText(), expected);
        }

        public static void CompareContent(string testName, string actualResult, FileInfo expected)
        {
            var actualLines = SplitLines(actualResult);
            var expectedLines = expected?.ReadAllLines();
            CompareLines(testName, actualLines, expectedLines, expected);
        }

        public static void CompareContent(string testName, string actualResult, string expectedResult)
        {
            var actualLines = SplitLines(actualResult);
            var expectedLines = SplitLines(expectedResult);
            CompareLines(testName, actualLines, expectedLines, null);
        }

        public static void CompareLines(string testName, string[] actualLines, string[] expectedLines, FileInfo expected)
        {
            bool autoReplace = false;

            if (testName == null && actualLines == null && expectedLines == null && expected == null)
            {
                if (autoReplace)
                    Assert.Fail("Set AutoReplace to false in TestUtils.CompareLines()\n\n");
                else
                    return;
            }

            Assert.IsNotNull(testName);
            Assert.IsNotNull(actualLines);
            Assert.IsNotNull(expectedLines);

            var lineFaults = new List<int>();
            for (int c = 0; c < actualLines.Length && c < expectedLines.Length; c++)
            {
                var actualLine = actualLines[c];
                var expectedLine = expectedLines[c];
                if (actualLine != expectedLine)
                {
                    lineFaults.Add(c);
                }
            }

            if (lineFaults.Count > 0 || actualLines.Length != expectedLines.Length)
            {
                var errors = new StringBuilder();
                errors.AppendLine("result != expectedResult  In test:" + testName);
                if (lineFaults.Count > 0)
                {
                    errors.AppendLine("at line" + (lineFaults.Count > 1 ? "s" : "") + ": " + string.Join(",", lineFaults));
                }
                else
                {
                    errors.AppendLine($"line count differs: expecting {expectedLines.Length} lines but found {actualLines.Length} lines instead.");
                }

                if (autoReplace && expected != null)
                {
                    expected.WriteAllLines(actualLines);
                    errors.AppendLine("autoReplace is active ! Output file has been rewritten\n");
                    errors.AppendLine("Please rerun unit test\n");
                }
                else
                {
                    errors.AppendLine("See TestUtils.cs CompareLines method to auto-replace ExpectedResult");
                    errors.AppendLine("======= RESULT =======");
                    foreach (var actualLine in actualLines)
                    {
                        errors.AppendLine(actualLine);
                    }
                    errors.AppendLine("======================");
                }

                throw new Exception(errors.ToString());
            }
        }

        public static string GetReportDirectoryPath()
        {
            return Path.Combine(Directory.GetCurrentDirectory(), "PerformanceReports");
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
                                compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime +
                                compiler.PerfStatsForCodeQualityCheck.FirstCompilationTime;

                report.AppendLine("");
                report.AppendLine("First compilation performance");
                FormatLine(compiler.PerfStatsForText.FirstCompilationTime, totalTime, "text update");
                FormatLine(compiler.PerfStatsForScanner.FirstCompilationTime, totalTime, "scanner");
                FormatLine(compiler.PerfStatsForPreprocessor.FirstCompilationTime, totalTime, "preprocessor");
                FormatLine(compiler.PerfStatsForCodeElementsParser.FirstCompilationTime, totalTime, "code elements parser");
                FormatLine(compiler.PerfStatsForTemporarySemantic.FirstCompilationTime, totalTime, "temporary semantic class parser");
                FormatLine(compiler.PerfStatsForProgramCrossCheck.FirstCompilationTime, totalTime, "cross check class parser");
                FormatLine(compiler.PerfStatsForCodeQualityCheck.FirstCompilationTime, totalTime, "quality check class parser");

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
            FormatLine(stats.AverageQualityCheckerParserTime, stats.AverageTotalProcessingTime, "quality check class parser");
            FormatLine(stats.AverageDiagnosticCollectionTime, stats.AverageTotalProcessingTime, "diagnostics collection time");

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
                AverageTextUpdateTime = 0;
                AverageScannerTime = 0;
                AveragePreprocessorTime = 0;
                AverageCodeElementParserTime = 0;
                AverateTemporarySemanticsParserTime = 0;
                AverageCrossCheckerParserTime = 0;
                AverageQualityCheckerParserTime = 0;
                AverageDiagnosticCollectionTime = 0;
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
            public float AverageQualityCheckerParserTime { get; set; }
            public float AverageDiagnosticCollectionTime { get; set; }
            public float AverageTotalProcessingTime { get; set; }
            //Number of lines in Cobol file
            public int Line { get; set; }
            //Number of CodeElements found during the parsing
            public int TotalCodeElements { get; set; }
        }
    }
}

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Report;

namespace TypeCobol.Test.Report
{
    /// <summary>
    /// Report Tests helper functionalities.
    /// </summary>
    public class ReportTestHelper
    {
        private static string ROOT_INPUT = Path.Combine("Report", "Input");
        private static string ROOT_OUTPUT = Path.Combine("Report", "Output");
        private static string ROOT_COPY = Path.Combine("Report", "Copy");

        /// <summary>
        /// Return Code
        /// </summary>
        public enum ReturnCode
        {
            Success,
            ParserDiagnosticsErrors,//Failed with diagnostics errors
            NoReportFile,//No Report file generated.
        };

        /// <summary>
        /// Parse a file using an INodeListener and IReport instance and compare the resulting report.
        /// </summary>
        /// <param name="fileName">The file name to parse</param>
        /// <param name="isCopy">True to indicate the source file is a copy, False otherwise</param>
        /// <param name="reportFileName">The file that contains the expected report</param>
        /// <typeparam name="T">The Type of the IReport instance to be instantiated.</typeparam>
        /// <returns>Return true if the report has been generated and compared, false otherwise</returns>
        public static ReturnCode ParseWithNodeListenerReportCompare<T>(string fileName, bool isCopy, string reportFileName)
            where T : IReport, ISyntaxDrivenAnalyzer, new()
        {
            T report = default;
            var analyzerProvider = new AnalyzerProviderWrapper(str => Debug.Fail(str));
            analyzerProvider.AddActivator((o, t) => report = new T());

            string input = Path.Combine(ROOT_INPUT, fileName);
            string output = Path.Combine(ROOT_OUTPUT, reportFileName);
            DocumentFormat format = DocumentFormat.RDZReferenceFormat;
            var parser = new TypeCobol.Parser();
            var typeCobolOption = new TypeCobolOptions { ExecToStep = ExecutionStep.SemanticCrossCheck };
            
            string copyFolder = Path.Combine(Directory.GetCurrentDirectory(), ROOT_COPY);
            parser.Init(input, isCopy, typeCobolOption, format, new List<string>() { copyFolder }, analyzerProvider);
            parser.Parse(input);

            // warning diagnostics are not considered : for example, test with warning with COPY SUPPRESS is always running
            if (parser.Results.AllDiagnostics().All(d => d.Info.Severity == TypeCobol.Compiler.Diagnostics.Severity.Warning))
            {
                if (report != null)
                {
                    using (StringWriter sw = new StringWriter())
                    {
                        report.Report(sw);
                        // compare with expected result
                        string result = sw.ToString();
                        string expected = File.ReadAllText(output, format.Encoding);
                        TestUtils.CompareLines(input, result, expected, PlatformUtils.GetPathForProjectFile(output));
                        return ReturnCode.Success;
                    }
                }

                return ReturnCode.NoReportFile;
            }

            return ReturnCode.ParserDiagnosticsErrors;
        }
    }
}

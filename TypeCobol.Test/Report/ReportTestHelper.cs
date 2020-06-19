using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
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
        /// Parse an file using a NodeListener and IReport instance and compare the resulting report.
        /// </summary>
        /// <param name="fileName">The file name to parse</param>
        /// <param name="reportFileName">The file that contains the expected report</param>
        /// <param name="reportType">The Type of the IReport instance to be instantiated.</param>
        /// <returns>Return true if the report has been generated and compared, false otherwise</returns>
        public static ReturnCode ParseWithNodeListenerReportCompare(string fileName, string reportFileName, Type reportType)
        {
            Assert.IsTrue(Tools.Reflection.IsTypeOf(reportType, typeof(IReport)));
            IReport report = null;//Variable to receive the created report instance.     

            NodeListenerFactory factory = () =>
            {
                object obj = Activator.CreateInstance(reportType, args: Path.GetFullPath(reportFileName));
                Assert.IsTrue(obj is INodeListener);
                INodeListener nodeListener = (INodeListener) obj;
                Assert.IsTrue(nodeListener is IReport);
                report = (IReport) nodeListener;
                return nodeListener;
            };

            //Register the Node Listener Factory
            NodeDispatcher.RegisterStaticNodeListenerFactory(factory);

            try
            {
                string input = Path.Combine(ROOT_INPUT, fileName);
                string output = Path.Combine(ROOT_OUTPUT, reportFileName);
                DocumentFormat format = DocumentFormat.RDZReferenceFormat;
                var parser = new TypeCobol.Parser();
                var typeCobolOption = new TypeCobolOptions { ExecToStep = ExecutionStep.CrossCheck };
#if EUROINFO_RULES
                typeCobolOption.AutoRemarksEnable = false;
#endif
                String copyFolder = Path.Combine(Directory.GetCurrentDirectory(), ROOT_COPY);
                parser.Init(input, typeCobolOption, format, new List<string>() { copyFolder });
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
                            TestUtils.compareLines(input, result, expected, PlatformUtils.GetPathForProjectFile(output));
                            return ReturnCode.Success;
                        }
                    }
                    else
                    {
                        return ReturnCode.NoReportFile;
                    }
                }
                else
                {
                   return ReturnCode.ParserDiagnosticsErrors;
                }
            }
            finally
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(factory);
            }
        }
    }
}

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Report;
using TypeCobol.Test.Utils;

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
        /// Parse an file using a NodeListener and IReport instance and compare the resulting report.
        /// </summary>
        /// <typeparam name="TCtx"></typeparam>
        /// <param name="fileName">The file name to parse</param>
        /// <param name="reportFileName">The file that contains the expected report</param>
        /// <param name="reportType">The Type of the IReport instance to be instantiated.</param>
        public static void ParseWithNodeListenerReportCompare<TCtx>(string fileName, string reportFileName, System.Type reportType) where TCtx : class
        {
            Assert.IsTrue(Tools.Reflection.IsTypeOf(reportType, typeof(IReport)));
            IReport report = null;//Variable to receive the created report instance.            
            TypeCobol.Compiler.Parser.NodeListenerFactory<TCtx> factory = () =>
            {
                object obj = System.Activator.CreateInstance(reportType);
                Assert.IsTrue(obj is NodeListener<TCtx>);
                TypeCobol.Compiler.Parser.NodeListener<TCtx> nodeListener = (TypeCobol.Compiler.Parser.NodeListener<TCtx>)obj;
                Assert.IsNotNull(nodeListener);
                report = (IReport)nodeListener;
                return nodeListener;
            };

            //Register the Node Listener Factory
            TypeCobol.Compiler.Parser.NodeDispatcher<TCtx>.RegisterStaticNodeListenerFactory(factory);

            try
            {
                string input = Path.Combine(ROOT_INPUT, fileName);
                string output = Path.Combine(ROOT_OUTPUT, reportFileName);
                DocumentFormat format = DocumentFormat.RDZReferenceFormat;
                var parser = new TypeCobol.Parser();
                var typeCobolOption = new TypeCobolOptions { ExecToStep = ExecutionStep.CrossCheck };
#if EUROINFO_RULES
                bool autoRemarks = false;
                typeCobolOption.AutoRemarksEnable = autoRemarks;
#endif
                String copyFolder = Path.Combine(Directory.GetCurrentDirectory(), ROOT_COPY);
                parser.Init(input, typeCobolOption, format, new List<string>() { copyFolder });
                parser.Parse(input);

                var allDiags = parser.Results.AllDiagnostics();
                if (allDiags.Count == 0)
                {
                    if (report != null)
                    {
                        using (System.IO.StringWriter sw = new StringWriter())
                        {
                            report.Report(sw);
                            // compare with expected result
                            string result = sw.ToString();
                            string expected = File.ReadAllText(output, format.Encoding);
                            TypeCobol.Test.TestUtils.compareLines(input, result, expected, PlatformUtils.GetPathForProjectFile(output));
                        }
                    }
                }
            }
            finally
            {
                TypeCobol.Compiler.Parser.NodeDispatcher<TCtx>.RemoveStaticNodeListenerFactory(factory);
            }
        }
    }
}

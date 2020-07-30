using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis.Report;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// Dfa test for call module report
    /// </summary>
    [TestClass]
    public class DfaCallPgmReport
    {
        [TestMethod]
        public void InBulkCallPgmReportTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Report", "InBulkCallPgm.cbl");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = CfgTestUtils.ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfgs.Count == 1);

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(cfgs);
            using (System.IO.StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(Directory.GetCurrentDirectory(), "Report", "InBulkCallPgm.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }

        [TestMethod]
        public void InBulkCallPgm88SetReportTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Report", "InBulkCallPgm88Set.cbl");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = CfgTestUtils.ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfgs.Count == 1);

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(cfgs);
            using (System.IO.StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(Directory.GetCurrentDirectory(), "Report", "InBulkCallPgm88Set.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }
    }
}

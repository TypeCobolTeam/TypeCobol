using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Report;
using TypeCobol.Compiler.Symbols;

using static TypeCobol.Analysis.Test.CfgTestUtils;

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
            string path = Path.Combine(CfgTestUtils.Report, "InBulkCallPgm.cbl");
            var cfgs = ParseCompareDiagnostics<DfaBasicBlockInfo<VariableSymbol>>(path, CfgBuildingMode.WithDfa);
            Assert.IsTrue(cfgs.Count == 1);

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(cfgs);
            using (StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(CfgTestUtils.Report, "InBulkCallPgm.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }

        [TestMethod]
        public void InBulkCallPgm88SetReportTest()
        {
            string path = Path.Combine(CfgTestUtils.Report, "InBulkCallPgm88Set.cbl");
            var cfgs = ParseCompareDiagnostics<DfaBasicBlockInfo<VariableSymbol>>(path, CfgBuildingMode.WithDfa);
            Assert.IsTrue(cfgs.Count == 1);

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(cfgs);
            using (StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(CfgTestUtils.Report, "InBulkCallPgm88Set.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }

        [TestMethod]
        public void ProcCallPgmReportTest()
        {
            string path = Path.Combine(CfgTestUtils.Report, "ProcCallPgm.cbl");
            DfaTestResults dfaresult = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaresult.Graphs.Count == 1);

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(dfaresult.Graphs);
            using (StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(CfgTestUtils.Report, "ProcCallPgm.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }

    }
}

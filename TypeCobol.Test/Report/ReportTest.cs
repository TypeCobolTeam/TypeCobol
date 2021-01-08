using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Report;

namespace TypeCobol.Test.Report
{
    [TestClass]
    public class ReportTest
    {
        [TestMethod]
        [TestCategory("Report")]
        public void TestReportMoveInitializeInCopy()
        {
            Assert.AreEqual(ReportTestHelper.ReturnCode.Success, ReportTestHelper.ParseWithNodeListenerReportCompare<CopyMoveInitializeReport>("RPTCPY01.cbl", "RPTCPY01.rpt"));
            Assert.AreEqual(ReportTestHelper.ReturnCode.ParserDiagnosticsErrors, ReportTestHelper.ParseWithNodeListenerReportCompare<CopyMoveInitializeReport>("RPTCPY02.cbl", "RPTCPY02.rpt"));
        }
    }
}

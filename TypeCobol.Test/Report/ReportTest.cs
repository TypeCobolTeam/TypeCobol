using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.Test.Report
{
    [TestClass]
    public class ReportTest
    {
        [TestMethod]
        [TestCategory("Report")]
        public void TestReportMoveInitializeInCopy()
        {
            Assert.AreEqual(ReportTestHelper.ReturnCode.Success, ReportTestHelper.ParseWithNodeListenerReportCompare("RPTCPY01.cbl", "RPTCPY01.rpt",
                typeof(TypeCobol.Compiler.Report.CopyMoveInitializeReport)));

            Assert.AreEqual(ReportTestHelper.ReturnCode.ParserDiagnosticsErrors, ReportTestHelper.ParseWithNodeListenerReportCompare("RPTCPY02.cbl", "RPTCPY02.rpt",
                typeof(TypeCobol.Compiler.Report.CopyMoveInitializeReport)));
        }

        [TestMethod]
        [TestCategory("Report")]
        public void TestReportCall()
        {
            Assert.AreEqual(ReportTestHelper.ReturnCode.Success, ReportTestHelper.ParseWithNodeListenerReportCompare("RPTCAL01.cbl", "RPTCAL01.rpt",
                typeof(TypeCobol.Compiler.Report.ZCallPgmReport)));
        }
    }
}

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
            Assert.IsTrue(ReportTestHelper.ParseWithNodeListenerReportCompare("RPTCPY01.cbl", "RPTCPY01.rpt",
                typeof(TypeCobol.Compiler.Report.CopyMoveInitializeReport)));

            Assert.IsFalse(ReportTestHelper.ParseWithNodeListenerReportCompare("RPTCPY02.cbl", "RPTCPY02.rpt",
                typeof(TypeCobol.Compiler.Report.CopyMoveInitializeReport)));
        }

        [TestMethod]
        [TestCategory("Report")]
        public void TestReportCall()
        {
            Assert.IsTrue(ReportTestHelper.ParseWithNodeListenerReportCompare("RPTCAL01.cbl", "RPTCAL01.rpt",
                typeof(TypeCobol.Compiler.Report.ZCallPgmReport)));
        }
    }
}

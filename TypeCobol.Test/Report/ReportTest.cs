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
            ReportTestHelper.ParseWithNodeListenerReportCompare<TypeCobol.Compiler.CodeElements.CodeElement>("RPTCPY01.cbl", "RPTCPY01.rpt",
                typeof(TypeCobol.Compiler.Report.CopyMoveInitializeReport<TypeCobol.Compiler.CodeElements.CodeElement>));
        }
    }
}

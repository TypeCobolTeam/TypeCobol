using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class LSRTest
    {
        [TestMethod]
        [Ignore]

        public void Incremental_RemoveDataDivisionCtrlZ()
        {
            LSRTestHelper.Test("Incremental_RemoveDataDivisionCtrlZ", true);
        }
    }
}

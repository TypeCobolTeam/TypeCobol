using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Threading;
using System.Globalization;

namespace TypeCobol.Test
{
    [TestClass]
    public class TestLauncher
    {
        [TestMethod]
        public void CheckFile()
        {
            TestCollection.CheckFile();
        }

        [TestMethod]
        public void CheckText()
        {
            TestCollection.CheckText();
        }

        [TestMethod]
        public void CheckScanner()
        {
            //Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
            TestCollection.CheckScanner();
        }

        [TestMethod]
        public void CheckPreprocessor()
        {
            TestCollection.CheckPreprocessor();
        }

		[TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
        public void CheckParser()
        {
            TestCollection.CheckParser();
        }
    }
}

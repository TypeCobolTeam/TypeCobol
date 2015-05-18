using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

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
            TestCollection.CheckScanner();
        }

        [TestMethod]
        public void CheckPreprocessor()
        {
            TestCollection.CheckPreprocessor();
        }

        [TestMethod]
        public void CheckParser()
        {
            TestCollection.CheckParser();
        }
    }
}

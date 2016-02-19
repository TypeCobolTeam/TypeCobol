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
        public void CheckParser()
        {
            TestCollection.CheckParser();
        }

        [TestMethod]
        public void CheckPipeline()
        {
            TestCollection.CheckPipeline();
        }

        [TestMethod]
        public void CheckUtils() {
            TestPictureParsing.Check_PictureToType();
        }
    }
}

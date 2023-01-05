using System;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test {

    [TestClass]
    public class NewTestsToFix
    {
        static string _Root = PlatformUtils.GetPathForProjectFile("NewTestsToFix" + Path.DirectorySeparatorChar + "Parser");

        [TestMethod]
        [TestCategory("NewTestsToFix-Parsing")]
        [TestProperty("Time", "fast")]
        [Ignore]
        public void NewCobol85TestsToFix()
        {
            string[] extensions = { ".cbl", ".pgm" };
            Console.WriteLine("Entering directory \"" + _Root + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(_Root, extensions);
            int nbOfTests = folderTester.Test();
            Console.Write("\n");

            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");

            //This test use TypeChecker which is specific to TypeCobol
            //As specifications of TypeCobol are not final yet this test can't be used
            //            TestChangeEvent.Check_ParserIntegration();
        }

        /// <summary>
        /// Check only files with *.tcbl extensions
        /// </summary>
        [TestMethod]
        [TestCategory("NewTestsToFix-Parsing")]
        [TestProperty("Time", "fast")]
        [Ignore]
        public void NewTcblTestsToFix()
        {
            string[] extensions = { ".tcbl" };
            Console.WriteLine("Entering directory \"" + _Root + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(_Root, extensions);
            int nbOfTests = folderTester.Test();
            Console.Write("\n");

            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }
    }
}

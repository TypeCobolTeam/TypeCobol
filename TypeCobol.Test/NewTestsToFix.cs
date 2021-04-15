using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test {

    [TestClass]
    public class NewTestsToFix
    {
        static string root = PlatformUtils.GetPathForProjectFile("NewTestsToFix" + Path.DirectorySeparatorChar + "Parser");
        static string sampleRoot = root + Path.DirectorySeparatorChar + "Samples";
        static string resultRoot = root + Path.DirectorySeparatorChar + "ResultFiles";

        [TestMethod]
        [TestCategory("NewTestsToFix-Parsing")]
        [TestProperty("Time", "fast")]
		[Ignore]
        public void NewCobol85TestsToFix()
        {
            int nbOfTests = 0;

            string[] extensions = { ".cbl", ".pgm" };
            foreach (string directory in GetCurrentAndSubDirectories(sampleRoot))
            {
                var dirname = Path.GetFileName(directory);

                Console.WriteLine("Entering directory \"" + dirname + "\" [" + string.Join(", ", extensions) +
                                         "]:");
                var folderTester = new FolderTester(sampleRoot, resultRoot, directory, extensions);
                folderTester.Test();
                nbOfTests += folderTester.GetTestCount();
                Console.Write("\n");
            }

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
            int nbOfTests = 0;
            string[] extensions = { ".tcbl" };

            foreach (string directory in GetCurrentAndSubDirectories(sampleRoot))
            {
                var dirname = Path.GetFileName(directory);
                
                Console.WriteLine("Entering directory \"" + dirname + "\" [" + string.Join(", ", extensions) + "]:");
                var folderTester = new FolderTester(sampleRoot, resultRoot, directory, extensions);
                folderTester.Test();
                nbOfTests += folderTester.GetTestCount();
                Console.Write("\n");
            }
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }

        public static IEnumerable<String> GetCurrentAndSubDirectories(string root)
        {
            yield return root;
            var directories =  Directory.GetDirectories(root);
            foreach (var dir in directories)
            {
                yield return dir;
            }
        }
    }
}

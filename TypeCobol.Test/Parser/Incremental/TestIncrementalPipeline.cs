using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.Test.Parser.Incremental
{
    /// <summary>
    /// These tests are going to check if the document's line update works fine. 
    /// </summary>
    [TestClass]
    public class IncrementalTextLineChanges
    {
        private static readonly string _Root = PlatformUtils.GetPathForProjectFile(@"Parser\Incremental");

        private static void TestFolder([CallerMemberName] string folder = null)
        {
            Debug.Assert(folder != null);
            string path = Path.Combine(_Root, folder);
            string[] sourceExtensions = { ".tcbl", ".cbl" };
            var folderTester = new TypeCobol.Test.UtilsNew.FolderTester(path, sourceExtensions);
            int testCount = folderTester.Test();
            Console.WriteLine("Number of tests: " + testCount);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void TextLineChanges() => TestFolder();

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void BasicEdits() => TestFolder();

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Structural() => TestFolder();

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void AdvancedEdits() => TestFolder();
    }
}

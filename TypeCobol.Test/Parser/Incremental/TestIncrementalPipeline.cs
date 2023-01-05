using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser.Incremental
{
    /// <summary>
    /// These tests are going to check if the document's line update works fine. 
    /// </summary>
    [TestClass]
    public class IncrementalTextLineChanges
    {
        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void TestFolder()
        {
            string rootFolder = PlatformUtils.GetPathForProjectFile(@"Parser\Incremental\TextLineChanges");
            string[] sourceExtensions = { ".tcbl", ".cbl" };
            var folderTester = new FolderTester(rootFolder, sourceExtensions);
            int testCount = folderTester.Test();
            Console.WriteLine("Number of tests: " + testCount);
        }
    }
}

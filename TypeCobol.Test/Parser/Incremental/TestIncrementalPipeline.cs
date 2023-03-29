using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
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
        private static readonly string _Root = PlatformUtils.GetPathForProjectFile(@"Parser\Incremental");
        private static readonly string _RootPrograms = PlatformUtils.GetPathForProjectFile(@"Parser\Programs");

        private static void TestFolder([CallerMemberName] string folder = null)
        {
            Debug.Assert(folder != null);
            string path = Path.Combine(_Root, folder);
            string[] sourceExtensions = { ".tcbl", ".cbl" };
            var folderTester = new FolderTester(path, sourceExtensions);
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

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void ContinuationLines() => TestFolder();

        private static void TestProgramsWithChangesGenerator<TChangesGenerator>(params string[] exclude)
            where TChangesGenerator : IIncrementalChangesGenerator, new()
        {
            string[] extensions = { ".cbl", ".pgm", ".tcbl" };
            Console.WriteLine("Entering directory \"" + _RootPrograms + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(_RootPrograms, extensions, exclude: exclude) { ChangesGenerator = new TChangesGenerator() };
            int nbOfTests = folderTester.Test();
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddEmptyLineAtBeginningThenRemove() => TestProgramsWithChangesGenerator<AddEmptyLineAtBeginningThenRemove>();

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddEmptyLineAtEndThenRemove() => TestProgramsWithChangesGenerator<AddEmptyLineAtEndThenRemove>(
            "UseCopyWithReplaceDeclaredWithReplacing", "UseCopyWithReplaceDeclaredWithReplacing2" // TODO #2480
            );

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddEmptyLineInTheMiddleThenRemove() => TestProgramsWithChangesGenerator<AddEmptyLineInTheMiddleThenRemove>(
            "UseCopyWithReplaceDeclaredWithReplacing", "UseCopyWithReplaceDeclaredWithReplacing2" // TODO #2480
            );

        [TestMethod]
        [TestCategory("Incremental")]
        public void ClearDocumentThenRewriteLineByLine() => TestProgramsWithChangesGenerator<ClearDocumentThenRewriteLineByLine>(
                "UseCopyWithReplaceDeclaredWithReplacing", "UseCopyWithReplaceDeclaredWithReplacing2", // TODO #2480
                "DebugLinesNotDebugging" // TODO #2457
            );

        [TestMethod]
        [TestCategory("Incremental")]
        public void ClearDocumentThenRewriteLineByLineInReverseOrder() => TestProgramsWithChangesGenerator<ClearDocumentThenRewriteLineByLineInReverseOrder>(
                "UseCopyWithReplaceDeclaredWithReplacing", "UseCopyWithReplaceDeclaredWithReplacing2", // TODO #2480
                "IncompleteInspect", "Program", "SelectFile", "ContinuationLine9", "InitialValuesContinuations" // TODO #2467
            );
    }
}

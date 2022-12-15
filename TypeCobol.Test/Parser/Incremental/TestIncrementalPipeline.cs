using System;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;

namespace TypeCobol.Test.Parser.Incremental
{
    [TestClass]
    public class TestChangeEvent
    {
        private static readonly string Root = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Programs" + Path.DirectorySeparatorChar + "Cobol85";

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "long")]
        public void Check_BeforeAfterInsertionBatched()
        {
            var unitTest = new TypeCobol.Test.UtilsNew.TestUnit(Path.Combine(Root, "Simple.pgm"), DocumentFormat.FreeUTF8Format);

            var inputChangesFilePath = Path.Combine(Root, "Simple_EndProgramAddProgramId.inc");
            foreach (var inputChange in TypeCobol.Test.UtilsNew.InputChange.Load(inputChangesFilePath))
            {
                unitTest.AddInputChange(inputChange);
            }

            var formatter = new TypeCobol.Test.UtilsNew.CodeElements();
            var first = Path.Combine(Root, "Simple.1.txt");
            unitTest.AddComparison(new TypeCobol.Test.UtilsNew.Comparison("1", first, formatter));
            var second = Path.Combine(Root, "Simple.2.txt");
            unitTest.AddComparison(new TypeCobol.Test.UtilsNew.Comparison("2", second, formatter));

            unitTest.Run();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "long")]
        public void Check_BeforeAfterInsertion()
        {
            var unitTest = new TypeCobol.Test.UtilsNew.TestUnit(Path.Combine(Root, "Simple.pgm"), DocumentFormat.FreeUTF8Format);

            var inputChangesFilePath = Path.Combine(Root, "Simple_EndProgramAddProgramIdClear.inc");
            foreach (var inputChange in TypeCobol.Test.UtilsNew.InputChange.Load(inputChangesFilePath))
            {
                unitTest.AddInputChange(inputChange);
            }

            var formatter = new TypeCobol.Test.UtilsNew.CodeElements();
            var initial = Path.Combine(Root, "Simple.0.txt");
            unitTest.AddComparison(new TypeCobol.Test.UtilsNew.Comparison(null, initial, formatter));
            var first = Path.Combine(Root, "Simple.1.txt");
            unitTest.AddComparison(new TypeCobol.Test.UtilsNew.Comparison("1", first, formatter));
            var second = Path.Combine(Root, "Simple.2.txt");
            unitTest.AddComparison(new TypeCobol.Test.UtilsNew.Comparison("2", second, formatter));
            var third = Path.Combine(Root, "Simple.3.txt");
            unitTest.AddComparison(new TypeCobol.Test.UtilsNew.Comparison("3", third, formatter));

            unitTest.Run();
        }
    }

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
            string[] sourceExtensions = { ".tcbl" };
            var folderTester = new TypeCobol.Test.UtilsNew.FolderTester(rootFolder, sourceExtensions);
            int testCount = folderTester.Test();
            Console.WriteLine("Number of tests: " + testCount);
        }
    }
}

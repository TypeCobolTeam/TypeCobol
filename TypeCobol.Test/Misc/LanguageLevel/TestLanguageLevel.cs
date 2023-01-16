using System;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    [TestClass]
    public class TestLanguageLevel
    {
        private static readonly string _Folder = PlatformUtils.GetPathForProjectFile(Path.Combine("Misc", "LanguageLevel"));

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckUnsupportedLanguageFeaturesInCobol85Code()
        {
            int nbOfTests = 0;

            string[] extensions = { ".cbl" };
            var dirname = Path.GetFileName(_Folder);

            Console.WriteLine("Entering directory \"" + dirname + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(_Folder, extensions);
            nbOfTests += folderTester.Test(isCobolLanguage: true);
            Console.Write("\n");
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }
    }
}

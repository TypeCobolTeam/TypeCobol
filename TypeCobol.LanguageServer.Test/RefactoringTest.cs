using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.LanguageServer.Test.RefactoringTests;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class RefactoringTest
    {
        private static void TestDirectory([CallerMemberName] string directoryName = null)
        {
            Debug.Assert(directoryName != null);
            string path = Path.GetFullPath("RefactoringTests");
            path = Path.Combine(path, directoryName);

            var failedTests = new List<(string TestName, Exception Exception)>();
            foreach (var testFile in Directory.GetFiles(path))
            {
                try
                {
                    var refactoringProcessorTest = RefactoringProcessorTest.LoadFrom(testFile);
                    refactoringProcessorTest.Run();
                }
                catch (Exception exception)
                {
                    string testName = Path.GetFileNameWithoutExtension(testFile);
                    failedTests.Add((testName, exception));
                }
            }

            // Fail if errors encountered
            if (failedTests.Count > 0)
            {
                Console.WriteLine();
                foreach (var failedTest in failedTests)
                {
                    Console.WriteLine($"[{failedTest.TestName}] failed !");
                    Console.WriteLine(failedTest.Exception.Message);
                    Console.WriteLine();
                }

                Assert.Fail($"{failedTests.Count} test(s) failed !");
            }
        }

        [TestMethod]
        public void AdjustFillers() => TestDirectory();
    }
}

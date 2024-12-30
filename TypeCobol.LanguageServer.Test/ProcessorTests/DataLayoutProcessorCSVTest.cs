using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Test;
using TypeCobol.Test.Utils;

namespace TypeCobol.LanguageServer.Test.ProcessorTests
{
    /// <summary>
    /// Tests for DataLayout processor with CSV
    /// </summary>
    [TestClass]
    public class DataLayoutProcessorCSVTest
    {
        private const string RELATIVE_PATH = "DataLayout";
        private const string ROOT_PATH = "ProcessorTests";
        private readonly DataLayoutProcessor _processor = new();

        private static void DoTestProcessor(string sourceFileName, string expectedFileName, Func<CompilationUnit, (string Header, string[] Rows)> executeProcessor, [CallerMemberName] string testName = null)
        {
            // Parse source file
            var folder = PlatformUtils.GetPathForProjectFile(RELATIVE_PATH, Path.GetFullPath(ROOT_PATH));
            var compilationUnit = ParserUtils.ParseCobolFile(sourceFileName, folder, execToStep: ExecutionStep.SemanticCrossCheck);

            // Execute processor
            var processorResult = executeProcessor(compilationUnit);

            // Build actual result
            var result = new StringBuilder();
            result.AppendLine(processorResult.Header);
            foreach (var row in processorResult.Rows)
            {
                result.AppendLine(row);
            }

            // Compare to expected
            var expectedPath =  $"{Path.Combine(folder, expectedFileName)}.txt";
            var expected = File.ReadAllText(expectedPath);
            TestUtils.CompareLines(testName, result.ToString(), expected, expectedPath);
        }

        [TestMethod]
        public void TestSimple() => DoTestProcessor("simple", "CSV-simple", ExecuteProcessor);

        [TestMethod]
        public void TestStackedAndNested() => DoTestProcessor("stackedAndNested", "CSV-stackedAndNested", ExecuteProcessor);
        
        private (string Header, string[] Rows) ExecuteProcessor(CompilationUnit compilationUnit)
        {
            return _processor.GetDataLayoutAsCSV(compilationUnit, ";");
        }
    }
}

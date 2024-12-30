using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.Test.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;
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
        private const string TEST_DATA_PREFIX = "CSV-";
        private const string RELATIVE_PATH = "DataLayout";
        private const string ROOT_PATH = "ProcessorTests";
        private readonly DataLayoutProcessor _processor = new();

        private void DoTestProcessor(string sourceFileName, bool isCopy = false, [CallerMemberName] string testName = null)
        {
            // Parse source file
            var folder = PlatformUtils.GetPathForProjectFile(RELATIVE_PATH, Path.GetFullPath(ROOT_PATH));
            var compilationUnit = ParserUtils.ParseCobolFile(sourceFileName, folder, isCopy, execToStep: ExecutionStep.SemanticCrossCheck);

            // Parse data file
            var testDataFilePath = $"{Path.Combine(folder, TEST_DATA_PREFIX + testName)}.txt";
            var testData = ParseContent(testDataFilePath);

            // Retrieve unique argument = position
            var position = JToken.Parse(testData.Argument).ToObject<Position>();

            // Execute processor
            var processorResult = ExecuteProcessor(compilationUnit, position);

            // Build actual result
            var result = new StringBuilder();
            result.AppendLine(processorResult.Root);
            result.AppendLine(processorResult.Header);
            foreach (var row in processorResult.Rows)
            {
                result.AppendLine(row);
            }

            // Compare to expected
            var expected = testData.Expected;
            TestUtils.CompareLines(testName, result.ToString(), expected, null);
        }

        private (string Root, string Header, string[] Rows) ExecuteProcessor(CompilationUnit compilationUnit, Position position)
        {
            return _processor.GetDataLayoutAsCSV(compilationUnit, position, ";");
        }

        private static (string Argument, string Expected) ParseContent(string testDataFilePath)
        {
            var parts = LanguageServerTestUtils.ParseMultiplePartsContent(testDataFilePath);

            return (parts[0], parts[1]);
        }

        [TestMethod]
        public void Copy() => DoTestProcessor("copy", true);

        [TestMethod]
        public void SimplePgm() => DoTestProcessor("simplePgm");

        [TestMethod]
        public void MainPgm() => DoTestProcessor("stackedAndNestedPgm");

        [TestMethod]
        public void StackedPgm() => DoTestProcessor("stackedAndNestedPgm");

        [TestMethod]
        public void NestedPgm() => DoTestProcessor("stackedAndNestedPgm");
    }
}

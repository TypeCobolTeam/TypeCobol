using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Report;
using TypeCobol.LanguageServer.Test.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.Test;
using TypeCobol.Test.Utils;

namespace TypeCobol.LanguageServer.Test.ProcessorTests
{
    /// <summary>
    /// Tests for DataLayout processor with Tree
    /// </summary>
    [TestClass]
    public class DataLayoutProcessorTreeTest
    {
        private const string TEST_DATA_PREFIX = "Tree-";
        private const string RELATIVE_PATH = "DataLayout";
        private const string ROOT_PATH = "ProcessorTests";
        private readonly DataLayoutProcessor _processor = new();

        private void DoTestProcessor(string sourceFileName, bool isCopy = false, [CallerMemberName] string testName = null)
        {
            // Force ignoring null value in JSON
            var backupJsonSettings = JsonConvert.DefaultSettings;
            JsonConvert.DefaultSettings = () => new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore
            };

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

            // Build actual result (JSON)
            var result = JToken.FromObject(processorResult).ToString(Formatting.Indented);

            // Compare to expected (JSON) but before restore JSON settings
            var expectedJson = JToken.Parse(testData.Expected);
            var expected = JToken.FromObject(expectedJson).ToString(Formatting.Indented);
            JsonConvert.DefaultSettings = backupJsonSettings;
            TestUtils.CompareContent(testName, result, expected);
        }

        private DataLayoutNode ExecuteProcessor(CompilationUnit compilationUnit, Position position)
        {
            return _processor.GetDataLayoutAsTree(compilationUnit, position);
        }

        private static (string Argument, string Expected) ParseContent(string testDataFilePath)
        {
            var parts = LanguageServerTestUtils.ParseMultiplePartsContent(testDataFilePath);

            return (parts[0], parts[1]);
        }

        [TestMethod]
     
        public void Copy() => DoTestProcessor("copy", true);

        [TestMethod]
        public void MiscPgm() => DoTestProcessor("miscPgm");

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

using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Test.ProcessorTests
{
    /// <summary>
    /// Tests for DataLayout processor with Tree
    /// </summary>
    [TestClass]
    public class DataLayoutProcessorTreeTest : DataLayoutProcessorTest
    {
        private Func<JsonSerializerSettings> backupJsonSettings;

        protected override string GetTestPrefix() => "Tree-";

        protected override string GetActualResult(CompilationUnit compilationUnit, Position position)
        {
            // Execute processor
            var processorResult = _processor.GetDataLayoutAsTree(compilationUnit, position);

            // Build actual result (JSON)
            return JToken.FromObject(processorResult).ToString(Formatting.Indented);
        }

        protected override string GetExpectedResult(string expected)
        {
            // Build expected result (JSON)
            var expectedJson = JToken.Parse(expected);
            return JToken.FromObject(expectedJson).ToString(Formatting.Indented);
        }

        [TestInitialize]
        public void Initialize()
        {
            // Back-up JSON default settings
            backupJsonSettings = JsonConvert.DefaultSettings;

            // Force ignoring null value in JSON
            JsonConvert.DefaultSettings = () => new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore
            };
        }

        [TestCleanup]
        public void Cleanup()
        {
            // Restore JSON default settings
            JsonConvert.DefaultSettings = backupJsonSettings;
        }

        [TestMethod]
     
        public void Copy() => ExecuteTest("copy", true);

        [TestMethod]
        public void MiscPgm() => ExecuteTest("miscPgm");

        [TestMethod]
        public void SimplePgm() => ExecuteTest("simplePgm");

        [TestMethod]
        public void MainPgm() => ExecuteTest("stackedAndNestedPgm");

        [TestMethod]
        public void StackedPgm() => ExecuteTest("stackedAndNestedPgm");

        [TestMethod]
        public void NestedPgm() => ExecuteTest("stackedAndNestedPgm");
    }
}

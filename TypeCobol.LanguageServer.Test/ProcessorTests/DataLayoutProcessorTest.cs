using System.Runtime.CompilerServices;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.Test.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.Test;
using TypeCobol.Test.Utils;

namespace TypeCobol.LanguageServer.Test.ProcessorTests
{
    /// <summary>
    /// Abstract test class for DataLayout processor
    /// </summary>
    public abstract class DataLayoutProcessorTest
    {
        private const string RELATIVE_PATH = "DataLayout";
        private const string ROOT_PATH = "ProcessorTests";
        protected readonly DataLayoutProcessor _processor = new();

        protected void ExecuteTest(string sourceFileName, bool isCopy = false, [CallerMemberName] string testName = null)
        {
            // Parse source file
            var folder = PlatformUtils.GetPathForProjectFile(RELATIVE_PATH, Path.GetFullPath(ROOT_PATH));
            var compilationUnit = ParserUtils.ParseCobolFile(sourceFileName, folder, isCopy, execToStep: ExecutionStep.SemanticCrossCheck);

            // Parse data file
            var testDataFilePath = $"{Path.Combine(folder, GetTestPrefix() + testName)}.txt";
            var testData = ParseContent(testDataFilePath);

            // Retrieve unique argument = position
            var position = JToken.Parse(testData.Argument).ToObject<Position>();

            // Execute processor and build actual result
            var actual = GetActualResult(compilationUnit, position);

            // Compare to expected
            var expected = GetExpectedResult(testData.Expected);
            TestUtils.CompareContent(testName, actual, expected);
        }

        protected abstract string GetTestPrefix();

        protected abstract string GetActualResult(CompilationUnit compilationUnit, Position position);

        protected abstract string GetExpectedResult(string expected);

        private static (string Argument, string Expected) ParseContent(string testDataFilePath)
        {
            var parts = LanguageServerTestUtils.ParseMultiplePartsContent(testDataFilePath);

            return (parts[0], parts[1]);
        }
    }
}

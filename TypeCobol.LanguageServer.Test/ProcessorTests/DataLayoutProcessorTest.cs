using System.Runtime.CompilerServices;
using System.Text;
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
            var testDataFilePath = $"{Path.Combine(folder, TestPrefix + testName)}.txt";
            var testData = LanguageServerTestUtils.ParseMultiplePartsContent(testDataFilePath);

            // Retrieve unique argument = position
            var position = JToken.Parse(testData[0]).ToObject<Position>();

            // Execute processor and build actual result, catch error if any
            string actual;
            try
            {
                actual = GetActualResult(compilationUnit, position);
            }
            catch (Exception exception)
            {
                var builder = new StringBuilder();
                builder.AppendLine(exception.GetType().FullName);
                builder.AppendLine(exception.Message);
                actual = builder.ToString();
            }

            // Compare to expected
            var expected = FormatExpectedResult(testData[1]);
            TestUtils.CompareContent(testName, actual, expected);
        }

        protected abstract string TestPrefix { get; }

        protected abstract string GetActualResult(CompilationUnit compilationUnit, Position position);

        protected abstract string FormatExpectedResult(string expected);
    }
}

using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
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
        private const string RELATIVE_PATH = "DataLayout";
        private const string ROOT_PATH = "ProcessorTests";
        private readonly DataLayoutProcessor _processor = new();

        private static void DoTestProcessor(string sourceFileName, string testDataFileName, Func<CompilationUnit, Position, (string Header, string[] Rows)> executeProcessor, bool isCopy = false, [CallerMemberName] string testName = null)
        {
            // Parse source file
            var folder = PlatformUtils.GetPathForProjectFile(RELATIVE_PATH, Path.GetFullPath(ROOT_PATH));
            var compilationUnit = ParserUtils.ParseCobolFile(sourceFileName, folder, isCopy, execToStep: ExecutionStep.SemanticCrossCheck);

            // Parse data file
            var testDataFilePath = $"{Path.Combine(folder, testDataFileName)}.txt";
            var testData = ParseContent(testDataFilePath);

            // Retrieve arguments
            var arguments = JArray.Parse(testData.Arguments).Cast<object>().ToArray(); // Array of JToken, explicit cast to object is to avoid warning on conversion when calling the constructor
            if (arguments == null || arguments.Length == 0 || arguments[0] is not JObject jObject)
            {
                throw new ArgumentException($"Invalid arguments for test {testDataFileName}");
            }

            // Execute processor
            var processorResult = executeProcessor(compilationUnit, jObject.ToObject<Position>());

            // Build actual result
            var result = new StringBuilder();
            result.AppendLine(processorResult.Header);
            foreach (var row in processorResult.Rows)
            {
                result.AppendLine(row);
            }

            // Compare to expected
            var expected = testData.Expected;
            TestUtils.CompareLines(testName, result.ToString(), expected, null);
        }

        private static (string Arguments, string Expected) ParseContent(string testDataFilePath)
        {
            string arguments = null;

            int state = 0; // Track current part of the document
            var builder = new StringBuilder();
            using (var reader = File.OpenText(testDataFilePath))
            {
                while (reader.ReadLine() is { } line) // Non-null pattern + variable definition
                {
                    if (line.All(c => c == '-'))
                    {
                        // The line is a separator, flush content read so far into correct variable
                        string currentPart = builder.ToString()[..^2]; // Remove trailing line break
                        builder.Clear();

                        switch (state)
                        {
                            case 0:
                                // Array of arguments for the processor
                                arguments = currentPart;
                                break;
                        }

                        // Discard separator line but move on to next state
                        state++;
                        continue;
                    }

                    // Accumulate line
                    builder.AppendLine(line);
                }
            }

            // Last part is the expected
            string expected = builder.ToString(); // Keep trailing line break as we'll end up with one added in the actual result too

            return (arguments, expected);
        }

        [TestMethod]
        public void TestCopy() => DoTestProcessor("copy", "CSV-copy", ExecuteProcessor, true);

        [TestMethod]
        public void TestSimplePgm() => DoTestProcessor("simplePgm", "CSV-simplePgm", ExecuteProcessor);

        [TestMethod]
        public void TestMainPgm() => DoTestProcessor("stackedAndNestedPgm", "CSV-mainPgm", ExecuteProcessor);

        [TestMethod]
        public void TestStackedPgm() => DoTestProcessor("stackedAndNestedPgm", "CSV-stackedPgm", ExecuteProcessor);

        [TestMethod]
        public void TestNestedPgm() => DoTestProcessor("stackedAndNestedPgm", "CSV-nestedPgm", ExecuteProcessor);

        private (string Header, string[] Rows) ExecuteProcessor(CompilationUnit compilationUnit, Position position)
        {
            return _processor.GetDataLayoutAsCSV(compilationUnit, position, ";");
        }
    }
}

using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using TypeCobol.LanguageServer.Test.Utilities;
using TypeCobol.Test.Utils;
using TypeCobol.Test;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.Processor;
using Newtonsoft.Json.Linq;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Test.ProcessorTests
{
    [TestClass]
    public class CompletionProcessorTest
    {
        private const string RELATIVE_PATH = "Completion";
        private const string ROOT_PATH = "ProcessorTests";

        /*
         * Using a blank context as it is required by the processor but it won't matter during the tests
         * as each test is independent.
         */
        protected readonly CompletionProcessor _processor = new(new SignatureCompletionContext());

        private void ExecuteTest([CallerMemberName] string sourceFileName = null)
        {
            Debug.Assert(sourceFileName != null);

            // Parse test data file
            var folder = PlatformUtils.GetPathForProjectFile(RELATIVE_PATH, Path.GetFullPath(ROOT_PATH));
            var testDataFilePath = $"{Path.Combine(folder, sourceFileName)}.txt";
            var testData = LanguageServerTestUtils.ParseMultiplePartsContent(testDataFilePath);
            Debug.Assert(testData.Count == 3);
            string cobolString = testData[0];
            string completionRequest = testData[1];
            string expectedProposals = testData[2];

            // Parse original source code
            var options = new TypeCobolOptions();
            var format = DocumentFormat.RDZReferenceFormat;
            bool isCopy = !cobolString.TrimStart().StartsWith("IDENTIFICATION", StringComparison.OrdinalIgnoreCase); // Simple but should be enough, does not support copys starting with IDENTIFICATION...
            var compilationUnit = ParserUtils.ParseCobolString(cobolString, isCopy, options, format);

            // Get completion request argument (single arg is Position)
            var position = JToken.Parse(completionRequest).ToObject<Position>();

            // Execute processor and build actual result
            var completionItems = _processor.ComputeProposals(compilationUnit, position);
            string actualProposals = JToken.FromObject(completionItems, new JsonSerializer() { NullValueHandling = NullValueHandling.Ignore })
                .ToString(Formatting.Indented)
                + Environment.NewLine;

            // Compare to expected
            TestUtils.CompareContent(sourceFileName, actualProposals, expectedProposals);
        }

        [TestMethod]
        public void SimpleCompletionForVariable() => ExecuteTest();
    }
}

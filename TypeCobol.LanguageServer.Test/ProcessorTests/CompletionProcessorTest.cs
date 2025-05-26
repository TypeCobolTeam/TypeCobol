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
            /*
             * Assuming the source code is not part of a copy. Testing completion inside a copy
             * is not very meaningful as they are usually made of data definitions only.
             */
            var compilationUnit = ParserUtils.ParseCobolString(cobolString, false, options, format);

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

        [TestMethod]
        public void CompletionAfterUnsupportedKeyword() => ExecuteTest();
    }
}

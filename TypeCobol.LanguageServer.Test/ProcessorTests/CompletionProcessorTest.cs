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
            var sourceFilePath = Path.Combine(sourceFileName.Split('_'));
            var testDataFilePath = $"{Path.Combine(folder, sourceFilePath)}.txt";
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

        [TestMethod]
        public void AfterCall_WithoutUserFilterText() => ExecuteTest();

        [TestMethod]
        public void AfterCall_WithUserFilterText() => ExecuteTest();

        [TestMethod]
        public void AfterDisplay_NamesUsingHyphens() => ExecuteTest();

        [TestMethod]
        public void AfterDisplay_NamesUsingUnderscores() => ExecuteTest();

        [TestMethod]
        public void AfterInto_String() => ExecuteTest();

        [TestMethod]
        public void AfterInto_Unstring() => ExecuteTest();

        [TestMethod]
        public void AfterMove_NamesUsingHyphens() => ExecuteTest();

        [TestMethod]
        public void AfterMove_NamesUsingUnderscores() => ExecuteTest();

        [TestMethod]
        public void AfterOf_Chain1() => ExecuteTest();

        [TestMethod]
        public void AfterOf_Chain2() => ExecuteTest();

        [TestMethod]
        public void AfterOf_IfAddress() => ExecuteTest();

        [TestMethod]
        public void AfterOf_NoResult() => ExecuteTest();

        [TestMethod]
        public void AfterOf_SetAddress() => ExecuteTest();

        [TestMethod]
        public void AfterOf_SeveralParents() => ExecuteTest();

        [TestMethod]
        public void AfterOf_Variable() => ExecuteTest();

        [TestMethod]
        public void AfterPerform_NamesUsingHyphens() => ExecuteTest();

        [TestMethod]
        public void AfterPerform_NamesUsingUnderscores() => ExecuteTest();

        [TestMethod]
        public void AfterSet_NamesUsingHyphens() => ExecuteTest();

        [TestMethod]
        public void AfterSet_NamesUsingUnderscores() => ExecuteTest();

        [TestMethod]
        public void AfterTo_AddLiteral() => ExecuteTest();

        [TestMethod]
        public void AfterTo_AddVariable() => ExecuteTest();

        [TestMethod]
        public void AfterTo_Inspect() => ExecuteTest();

        [TestMethod]
        public void AfterTo_MoveLiteral() => ExecuteTest();

        [TestMethod]
        public void AfterTo_MoveSpaces() => ExecuteTest();

        [TestMethod]
        public void AfterTo_MoveVariable() => ExecuteTest();

        [TestMethod]
        public void AfterTo_MoveZero() => ExecuteTest();

        [TestMethod]
        public void AfterTo_Set() => ExecuteTest();

        [TestMethod]
        public void AfterTo_SetNothingAfterTo() => ExecuteTest();

        [TestMethod]
        public void AfterTo_SetOf() => ExecuteTest();
    }
}

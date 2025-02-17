using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.LanguageServer.Commands;
using TypeCobol.LanguageServer.Test.RefactoringTests;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.Test.Utils;

using Range = TypeCobol.LanguageServer.VsCodeProtocol.Range;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class RefactoringTest
    {
        /// <summary>
        /// Fake refactoring processor to test some hard-coded text edits
        /// </summary>
        private class FixedTextEditGenerator : IRefactoringProcessor
        {
            public FixedTextEditGenerator()
            {
                var builder = new StringBuilder();
                builder.AppendLine("      * 01: This line won't change");
                builder.AppendLine("      * 02: This line will have some text inserted");
                builder.AppendLine("      * 03: This line won't change");
                builder.AppendLine("      * 04: This line will have some text deleted");
                builder.AppendLine("      * 05: This line won't change");
                builder.AppendLine("      * 06: This line will be modified");
                builder.AppendLine("      * 07: This line won't change");
                builder.AppendLine("      * 08: This line will have some text inserted");
                builder.AppendLine("      * 09: This line won't change");
                builder.AppendLine("      * 10: This line will be partially deleted at its end");
                builder.AppendLine("      * 11: This line will be deleted");
                builder.AppendLine("      * 12: This line will be partially deleted at its beginning");
                builder.AppendLine("      * 13: This line won't change");
                builder.AppendLine("      * 14: This line will be partially modified");
                builder.AppendLine("      * 15: This line will be entirely replaced");
                builder.AppendLine("      * 16: This line will be partially modified");
                builder.AppendLine("      * 17: This line won't change");
                Target = ParserUtils.ParseCobolString(builder.ToString(), false, new TypeCobolOptions(), DocumentFormat.RDZReferenceFormat);

                builder.Clear();
                builder.AppendLine("refactoring.label=Generated some text edits on In-memory document");
                builder.AppendLine("refactoring.source=");
                builder.AppendLine("      * 01: This line won't change");
                builder.AppendLine("      * 02: This line [inserted text on single line] will have some text inserted");
                builder.AppendLine("      * 03: This line won't change");
                builder.AppendLine("      * 04: This line some text deleted");
                builder.AppendLine("      * 05: This line won't change");
                builder.AppendLine("      * 06: This line has been modified");
                builder.AppendLine("      * 07: This line won't change");
                builder.AppendLine("      * 08: This line [inserted text");
                builder.AppendLine("      * on multiple");
                builder.AppendLine("      * lines] will have some text inserted");
                builder.AppendLine("      * 09: This line won't change");
                builder.AppendLine("      * 10: This line will be partially deleted at its beginning");
                builder.AppendLine("      * 13: This line won't change");
                builder.AppendLine("      * 14: This line has been");
                builder.AppendLine("      * modified and the modification spans");
                builder.AppendLine("      * over multiple lines will be partially modified");
                builder.AppendLine("      * 17: This line won't change");
                builder.AppendLine("");
                ExpectedResult = builder.ToString();
            }

            public CompilationUnit Target { get; }

            public string ExpectedResult { get; }

            public IEnvironmentVariableProvider EnvironmentVariableProvider { get; set; }

            public TextDocumentIdentifier PrepareRefactoring(object[] arguments)
            {
                return new TextDocumentIdentifier(){ uri = nameof(FixedTextEditGenerator) };
            }

            public void CheckTarget(CompilationUnit compilationUnit)
            {
                if (compilationUnit != Target)
                    throw new InvalidOperationException("This processor can only be used on its own target !");
            }

            public (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit)
            {
                string label = $"Generated some text edits on {Target.TextSourceInfo.Name}";
                var textEdits = new List<TextEdit>();

                var insertSingleLine = TextEdit.Insert(new Position(){ line = 2, character = 22 }, "[inserted text on single line] ");
                textEdits.Add(insertSingleLine);

                var deleteSingleLine = TextEdit.Delete(Range.FromPositions(4, 22, 4, 32));
                textEdits.Add(deleteSingleLine);

                var modifySingleLine = TextEdit.Replace(Range.FromPositions(6, 22, 6, 29), "has been");
                textEdits.Add(modifySingleLine);

                var builder = new StringBuilder();
                builder.AppendLine("[inserted text");
                builder.AppendLine("      * on multiple");
                builder.Append("      * lines] ");
                var insertMultiLines = TextEdit.Insert(new Position() { line = 8, character = 22 }, builder.ToString());
                textEdits.Add(insertMultiLines);

                var deleteMultiLines = TextEdit.Delete(Range.FromPositions(10, 22, 12, 22));
                textEdits.Add(deleteMultiLines);

                builder.Clear();
                builder.AppendLine("has been");
                builder.AppendLine("      * modified and the modification spans");
                builder.Append("      * over multiple lines ");
                var modifyMultiLines = TextEdit.Replace(Range.FromPositions(14, 22, 16, 22), builder.ToString());
                textEdits.Add(modifyMultiLines);

                return (label, textEdits);
            }
        }

        private static void TestDirectory([CallerMemberName] string directoryName = null)
        {
            Debug.Assert(directoryName != null);
            string path = Path.GetFullPath("RefactoringTests");
            path = Path.Combine(path, directoryName);

            var failedTests = new List<(string TestFile, Exception Exception)>();
            foreach (var testFile in Directory.GetFiles(path, "*.*", SearchOption.AllDirectories))
            {
                try
                {
                    var refactoringProcessorTest = RefactoringProcessorTest.LoadFrom(testFile);
                    refactoringProcessorTest.Run();
                }
                catch (Exception exception)
                {
                    failedTests.Add((testFile, exception));
                }
            }

            // Fail if errors encountered
            if (failedTests.Count > 0)
            {
                Console.WriteLine();
                foreach (var failedTest in failedTests)
                {
                    Console.WriteLine($"[{failedTest.TestFile}] failed !");
                    Console.WriteLine(failedTest.Exception.Message);
                    Console.WriteLine(failedTest.Exception.StackTrace);
                    Console.WriteLine();
                }

                Assert.Fail($"{failedTests.Count} test(s) failed !");
            }
        }

        [TestMethod]
        public void TestServerSideApplyEdit()
        {
            // Manual test to check the behavior of the ApplyEdits mechanism
            var refactoringProcessor = new FixedTextEditGenerator();
            var refactoringProcessorTest = new RefactoringProcessorTest(nameof(TestServerSideApplyEdit), refactoringProcessor.Target, refactoringProcessor, null, refactoringProcessor.ExpectedResult);
            refactoringProcessorTest.Run();
        }

        [TestMethod]
        public void AdjustFillers() => TestDirectory();

        [TestMethod]
        public void InsertVariableDisplay() => TestDirectory();
    }
}

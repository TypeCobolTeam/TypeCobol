using System.Diagnostics;
using System.Text;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.LanguageServer.Commands.Refactor;
using TypeCobol.LanguageServer.Test.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.Test;
using TypeCobol.Test.Utils;

namespace TypeCobol.LanguageServer.Test.RefactoringTests
{
    internal class RefactoringProcessorTest
    {
        private static readonly Dictionary<string, IRefactoringProcessor> _RefactoringProcessors;

        static RefactoringProcessorTest()
        {
            _RefactoringProcessors = new Dictionary<string, IRefactoringProcessor>();

            // Index refactoring processor types by their full name
            var refactoringProcessorType = typeof(IRefactoringProcessor);
            var assembly = refactoringProcessorType.Assembly; // Consider types in TypeCobol.LanguageServer assembly
            foreach (var type in assembly.GetTypes())
            {
                if (type.GetInterfaces().Contains(refactoringProcessorType))
                {
                    // Found a type implementing directly IRefactoringProcessor
                    var instance = (IRefactoringProcessor)Activator.CreateInstance(type); // Expecting a parameterless constructor
                    Debug.Assert(type.FullName != null);
                    _RefactoringProcessors.Add(type.FullName, instance);
                }
            }
        }

        public static RefactoringProcessorTest LoadFrom(string testDataFilePath)
        {
            // Load file and split into parts
            string testName = Path.GetFileNameWithoutExtension(testDataFilePath);
            var testData = ParseContent(testDataFilePath);

            // Parse original source code
            var options = new TypeCobolOptions();
            var format = DocumentFormat.RDZReferenceFormat;
            bool isCopy = !testData.OriginalSource.TrimStart().StartsWith("IDENTIFICATION", StringComparison.OrdinalIgnoreCase); // Simple but should be enough, does not support copys starting with IDENTIFICATION...
            var target = ParserUtils.ParseCobolString(testData.OriginalSource, isCopy, options, format);

            // Identify processor to test
            var refactoringProcessor = _RefactoringProcessors[testData.ProcessorType];

            // Parse command arguments from JSON string
            var arguments = JArray.Parse(testData.CommandArguments).Cast<object>().ToArray(); // Array of JToken, explicit cast to object is to avoid warning on conversion when calling the constructor

            // Modified source as plain string
            string expectedResult = testData.ModifiedSource;

            return new RefactoringProcessorTest(testName, target, refactoringProcessor, arguments, expectedResult);
        }

        private static (string OriginalSource, string ProcessorType, string CommandArguments, string ModifiedSource) ParseContent(string testDataFilePath)
        {
            var parts = LanguageServerTestUtils.ParseMultiplePartsContent(testDataFilePath);

            return (parts[0], parts[1], parts[2], parts[3]);

        }

        internal RefactoringProcessorTest(string testName, CompilationUnit target, IRefactoringProcessor refactoringProcessor, object[] arguments, string expectedResult)
        {
            _testName = testName;
            _target = target;
            _refactoringProcessor = refactoringProcessor;
            _arguments = arguments;
            _expectedResult = expectedResult;
        }

        private readonly string _testName;
        private readonly CompilationUnit _target;
        private readonly IRefactoringProcessor _refactoringProcessor;
        private readonly object[] _arguments;
        private readonly string _expectedResult;

        public void Run()
        {
            // Pass args to processor
            var textDocumentIdentifier = _refactoringProcessor.PrepareRefactoring(_arguments);
            Console.WriteLine($"Testing {_refactoringProcessor.GetType().Name} on '{textDocumentIdentifier.uri}'...");

            // Validate refactoring
            _refactoringProcessor.CheckTarget(_target);

            // Perform refactoring
            var refactoring = _refactoringProcessor.PerformRefactoring(_target);

            // Apply text edits on original source code
            string actualResult = ApplyTextEdits(refactoring.Label, refactoring.TextEdits);

            // Compare actual modified code with expected modified code
            TestUtils.CompareLines(_testName, actualResult, _expectedResult, null);
        }

        private string ApplyTextEdits(string label, List<TextEdit> changes)
        {
            var result = new StringBuilder();
            result.AppendLine($"refactoring.label={label}");
            result.AppendLine("refactoring.source=");

            // Apply text edits on original source code: we simulate here what happens client-side
            // Note that this algorithm expects at most one change per line
            int changeIndex = 0;
            var nextChange = NextChange();
            for (int lineIndex = 0; lineIndex < _target.CobolTextLines.Count; lineIndex++)
            {
                string lineText = _target.CobolTextLines[lineIndex].Text;
                if (nextChange != null && lineIndex + 1 == nextChange.range.start.line)
                {
                    // The line is the target of the change
                    result.Append(lineText[..nextChange.range.start.character]); // Keep part of the first line before the change
                    result.Append(nextChange.newText); // Insert new text (maybe empty when it is a deletion)

                    // Skip lines overlapping the change
                    lineIndex += nextChange.range.end.line - nextChange.range.start.line;
                    lineText = _target.CobolTextLines[lineIndex].Text;

                    // Keep part of the last line after the change
                    result.AppendLine(lineText[nextChange.range.end.character..]);
                    nextChange = NextChange();
                }
                else
                {
                    // Line is unchanged
                    result.AppendLine(lineText);
                }
            }

            return result.ToString();

            TextEdit NextChange() => changeIndex < changes.Count ? changes[changeIndex++] : null;
        }
    }
}

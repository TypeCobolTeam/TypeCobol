using System.Diagnostics;
using System.Text;
using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.LanguageServer.Commands.Refactor;
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
            string originalSource = null;
            string processorType = null;
            string commandArguments = null;

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
                                // First part is the original source code
                                originalSource = currentPart;
                                break;
                            case 1:
                                // Second part is the processor .NET type name
                                processorType = currentPart;
                                break;
                            case 2:
                                // Third part is the array of arguments for the command/processor
                                commandArguments = currentPart;
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

            // Fourth and expectedly last part is the modified source code
            string modifiedSource = builder.ToString(); // Keep trailing line break as we'll end up with one added in the actual result too

            return (originalSource, processorType, commandArguments, modifiedSource);
        }

        private RefactoringProcessorTest(string testName, CompilationUnit target, IRefactoringProcessor refactoringProcessor, object[] arguments, string expectedResult)
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

            int lineIndex = 1;
            int changeIndex = 0;
            var nextChange = (changeIndex < changes.Count) ? changes[changeIndex++] : null;

            using (var lineEnumerator = _target.CobolTextLines.GetEnumerator())
            {
                while (lineEnumerator.MoveNext())
                {
                    Debug.Assert(lineEnumerator.Current != null);
                    string line = lineEnumerator.Current.Text;
                    if (nextChange != null && lineIndex == nextChange.range.start.line)
                    {
                        // Line is changed
                        if (IsInsert(nextChange))
                        {
                            // Insert new text
                            result.Append(line);
                            result.AppendLine(nextChange.newText);
                            lineIndex++;
                        }
                        else if (IsDelete(nextChange))
                        {
                            // Delete = ignore current line
                            lineIndex += nextChange.range.end.line - nextChange.range.start.line + 1;
                            for (int i = nextChange.range.start.line; i < nextChange.range.end.line; i++)
                            {
                                // And also ignore next lines (if needed)
                                lineEnumerator.MoveNext();
                            }
                        }
                        else
                        {
                            // Modified line = start of original line + new text + end of original line
                            // (which may be different from the start one if the change spread on several lines)
                            result.Append(line[..nextChange.range.start.character]);
                            result.Append(nextChange.newText);
                            for (int i = nextChange.range.start.line; i < nextChange.range.end.line; i++)
                            {
                                // Ignore replaced lines (if needed)
                                lineEnumerator.MoveNext();
                                line = lineEnumerator.Current.Text;
                            }
                            result.AppendLine(line[nextChange.range.end.character..]);
                            lineIndex++;
                        }

                        nextChange = (changeIndex < changes.Count) ? changes[changeIndex++] : null;
                    }
                    else
                    {
                        // Unchanged line
                        result.AppendLine(line);
                        lineIndex++;
                    }
                }
            }

            return result.ToString();
        }

        private static bool IsInsert(TextEdit change)
        {
            // start == end --> Insert
            return change.range.start.line == change.range.end.line && change.range.start.character == change.range.end.character;
        }

        private static bool IsDelete(TextEdit change)
        {
            // newText empty --> Delete
            return change.newText.Length == 0;
        }
    }
}

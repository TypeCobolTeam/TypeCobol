using System.Text;

namespace TypeCobol.LanguageServer.Test.Utilities
{
    internal static class LanguageServerTestUtils
    {
        /// <summary>
        /// Parse the content of a file into multiple parts. In the file the parts must be separated by lines with '-'.
        /// <br>Example for a test file:</br>
        /// <code>arguments</code>
        /// <code>---------------</code>
        /// <code>expected_result</code>
        /// <br>The result is 2 parts: {arguments, expected_result}</br>
        /// </summary>
        /// <param name="filePath">The path to the file to be parsed.</param>
        /// <returns>The parts as a list.</returns>
        public static List<string> ParseMultiplePartsContent(string filePath)
        {
            List<string> parts = new();

            var builder = new StringBuilder();
            using (var reader = File.OpenText(filePath))
            {
                while (reader.ReadLine() is { } line) // Non-null pattern + variable definition
                {
                    if (line.All(c => c == '-'))
                    {
                        // The line is a separator, flush content read so far into a part
                        parts.Add(builder.ToString()[..^Environment.NewLine.Length]); // Remove trailing line break
                        builder.Clear();

                        // Discard separator line but move on to next part
                        continue;
                    }

                    // Accumulate line
                    builder.AppendLine(line);
                }
            }

            // Last part
            parts.Add(builder.ToString()); // Keep trailing line break as we'll end up with one added in the actual result too

            return parts;
        }
    }
}

using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class CobolStringWriter : StringWriter
    {
        private int _lastNewLineEndIndex;

        public CobolStringWriter()
        {
            _lastNewLineEndIndex = -1;
        }

        public override void WriteLine(string value)
        {
            Write(value);
            WriteLine();
        }

        public override void WriteLine()
        {
            const string elevenSpaces = "           ";
            const string fourSpaces = "    ";
            const int maxStatementLength = CobolFormatAreas.End_B - CobolFormatAreas.Begin_B + 1; // Statements cannot exceed 61 characters
            const char debugIndicator = 'D';

            var builder = GetStringBuilder();
            int lineLength = builder.Length - _lastNewLineEndIndex - 1;
            if (lineLength > maxStatementLength)
            {
                // Get statement line and remove from builder
                string lastLineText = fourSpaces + builder.ToString(_lastNewLineEndIndex + 1, lineLength);
                builder.Remove(_lastNewLineEndIndex + 1, lineLength);

                // Split statement line and append
                var cobolLines = CobolTextLine.CreateCobolLines(ColumnsLayout.CobolReferenceFormat, new TypeCobolOptions(), 0, debugIndicator, string.Empty, lastLineText);
                var lines = cobolLines.Select(l => l.Text.TrimEnd()).ToArray();
                builder.AppendJoin(NewLine, lines);
            }
            else
            {
                // No need to split, but we still have to shift the statement in column 12
                builder.Insert(_lastNewLineEndIndex + 1, elevenSpaces);
            }

            // Keep index of the newline
            _lastNewLineEndIndex = builder.Length + NewLine.Length - 1;
            base.WriteLine();
        }
    }
}

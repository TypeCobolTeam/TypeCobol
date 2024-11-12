using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class CobolStringBuilder
    {
        private const int MAX_LENGTH = (int)CobolFormatAreas.End_B; // 72
        private const char ONE_SPACE = ' ';
        private const char DEBUG_INDICATOR = 'D';

        private static readonly string _SequenceNumber = new string(ONE_SPACE, CobolFormatAreas.EndNumber - CobolFormatAreas.BeginNumber + 1); // 6 spaces

        private readonly char _indicator;
        private readonly StringBuilder _text;
        private readonly StringBuilder _currentLine;
        private int _previousLineIndentLength;
        private bool _currentLineIsEmpty;

        public CobolStringBuilder(bool debug = false)
        {
            _indicator = debug ? DEBUG_INDICATOR : ONE_SPACE;
            _text = new StringBuilder();
            _currentLine = new StringBuilder();
            _previousLineIndentLength = -1;
            InitCurrentLine(false);
        }

        private void InitCurrentLine(bool useIndentFromPreviousLine)
        {
            _currentLine.Append(_SequenceNumber);
            _currentLine.Append(_indicator);
            if (useIndentFromPreviousLine)
            {
                Debug.Assert(_previousLineIndentLength >= 0);
                _currentLine.Append(new string(ONE_SPACE, _previousLineIndentLength));
            }
            
            _currentLineIsEmpty = true;
        }

        private void FlushCurrentLine()
        {
            string currentLine = _currentLine.ToString();
            _previousLineIndentLength = currentLine.Skip(_SequenceNumber.Length + 1).TakeWhile(c => c == ONE_SPACE).Count();
            _text.Append(currentLine);
            _currentLine.Clear();
        }

        public void AppendIndent(int length)
        {
            string indent = new string(ONE_SPACE, length);
            AppendText(indent);
        }

        public void AppendWord(string word)
        {
            AppendText(word);
            _currentLineIsEmpty = false;
        }

        private void AppendText(string text)
        {
            bool addSeparator = !_currentLineIsEmpty;
            int separatorLength = addSeparator ? 1 : 0;
            if (_currentLine.Length + separatorLength + text.Length > MAX_LENGTH)
            {
                AppendLine(true);
            }
            else if (addSeparator)
            {
                _currentLine.Append(ONE_SPACE);
            }

            _currentLine.Append(text);
        }

        public void AppendLine() => AppendLine(false);

        private void AppendLine(bool useIndentFromPreviousLine)
        {
            FlushCurrentLine();
            _text.AppendLine();
            InitCurrentLine(useIndentFromPreviousLine);
        }

        public override string ToString()
        {
            if (!_currentLineIsEmpty)
            {
                FlushCurrentLine();
            }

            return _text.ToString();
        }
    }
}

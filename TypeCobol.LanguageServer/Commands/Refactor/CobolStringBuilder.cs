using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    [DebuggerDisplay("Text = {_text}, CurrentLine = {_currentLine}")]
    internal class CobolStringBuilder
    {
        private const int MAX_LENGTH = (int)CobolFormatAreas.End_B; // 72
        private const char ONE_SPACE = ' ';
        private const char DEBUG_INDICATOR = 'D';

        private static readonly string _SequenceNumber = new string(ONE_SPACE, CobolFormatAreas.EndNumber - CobolFormatAreas.BeginNumber + 1); // 6 spaces

        private readonly char _indicator;
        private readonly StringBuilder _text;
        private readonly StringBuilder _currentLine;
        private bool _currentLineIsEmpty;

        public CobolStringBuilder(bool debug = false)
        {
            _indicator = debug ? DEBUG_INDICATOR : ONE_SPACE;
            _text = new StringBuilder();
            _currentLine = new StringBuilder();
            Clear();
        }

        private void InitCurrentLine()
        {
            _currentLine.Append(_SequenceNumber);
            _currentLine.Append(_indicator);
            _currentLineIsEmpty = true;
        }

        private int FlushCurrentLine()
        {
            string currentLine = _currentLine.ToString();
            int previousLineIndentLength = currentLine.Skip(_SequenceNumber.Length + 1).TakeWhile(c => c == ONE_SPACE).Count(); // TODO Add a param to the GetIndent method ?
            _text.Append(currentLine);
            _currentLine.Clear();
            return previousLineIndentLength;
        }

        public void AppendIndent(int length)
        {
            Debug.Assert(_currentLineIsEmpty);
            Debug.Assert(_currentLine.Length + length <= MAX_LENGTH);
            string indent = new string(ONE_SPACE, length);
            _currentLine.Append(indent);
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
                int indent = AppendLine();
                AppendIndent(indent);
            }
            else if (addSeparator)
            {
                _currentLine.Append(ONE_SPACE);
            }

            _currentLine.Append(text);
        }

        public void AppendLiteralForDisplay(string literalValue, char delimiter = '\'')
        {
            Debug.Assert(!_currentLineIsEmpty);

            bool addSeparator = true;
            int indent = _currentLine.Length - _SequenceNumber.Length;

            string text = literalValue;
            int remaining;
            int addedCharsCount;
            while (text.Length + (addedCharsCount = (addSeparator ? 3 : 2)) > (remaining = MAX_LENGTH - _currentLine.Length))
            {
                string part = text.Substring(0, remaining - addedCharsCount);
                if (addSeparator)
                {
                    _currentLine.Append(ONE_SPACE);
                }

                _currentLine.Append(delimiter + part + delimiter);

                AppendLine();
                AppendIndent(indent);

                text = text.Substring(part.Length);
                addSeparator = false;
            }

            if (text.Length > 0)
            {
                if (addSeparator)
                {
                    _currentLine.Append(ONE_SPACE);
                }

                _currentLine.Append(delimiter + text + delimiter);
                _currentLineIsEmpty = false;
            }
        }

        public int AppendLine()
        {
            int previousLineIndentLength = FlushCurrentLine();
            _text.AppendLine();
            InitCurrentLine();
            return previousLineIndentLength;
        }

        public void Clear()
        {
            _text.Clear();
            _currentLine.Clear();
            InitCurrentLine();
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

using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class CobolStringBuilder
    {
        private const int MAX_LENGTH = (int)CobolFormatAreas.End_B; // 72
        private const string SIX_SPACES = "      ";
        private const char ONE_SPACE = ' ';

        private readonly char _indicator;
        private readonly StringBuilder _text;
        private readonly StringBuilder _currentLine;
        private bool _currentLineIsEmpty;

        public CobolStringBuilder(char indicator = ONE_SPACE)
        {
            _indicator = indicator;
            _text = new StringBuilder();
            _currentLine = new StringBuilder();
            InitCurrentLine();
        }

        private void FlushCurrentLine()
        {
            _text.Append(_currentLine.ToString());
            _currentLine.Clear();
        }

        public void AppendWord(string word)
        {
            int wordLength = word.Length;
            if (wordLength > CobolFormatAreas.End_B - CobolFormatAreas.Begin_B + 1) // 61
            {
                throw new NotSupportedException("This build does not support word splitting !");
            }

            bool addSeparator = !_currentLineIsEmpty;
            int separatorLength = addSeparator ? 1 : 0;
            if (_currentLine.Length + separatorLength + wordLength > MAX_LENGTH)
            {
                AppendLine();
            }
            else if (addSeparator)
            {
                _currentLine.Append(ONE_SPACE);
            }

            _currentLine.Append(word);
            _currentLineIsEmpty = false;
        }

        public void AppendLine()
        {
            FlushCurrentLine();
            _text.AppendLine();
            InitCurrentLine();
        }

        private void InitCurrentLine()
        {
            _currentLine.Append(SIX_SPACES);
            _currentLine.Append(_indicator);
            _currentLineIsEmpty = true;
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

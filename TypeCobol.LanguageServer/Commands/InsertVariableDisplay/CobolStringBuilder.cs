using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Imitates StringBuilder but for generating Cobol code according to the RDz Reference Format.
    /// Internally it uses 2 StringBuilder: one for the accumulated text so far and one
    /// for the current line being built. When the current line exceeds the maximum allowed length,
    /// its content is transferred to the text and a new line is initialized.
    /// </summary>
    [DebuggerDisplay("Text = {_text}, CurrentLine = {_currentLine}")] // The ToString() method may alter the object, so we use custom debug display to avoid side effects
    internal class CobolStringBuilder
    {
        private const int MAX_LINE_LENGTH = (int)CobolFormatAreas.End_B; // 72
        private const char ONE_SPACE = ' ';
        private const char DEBUG_INDICATOR = 'D';
        private const char COMMENT_INDICATOR = '*';

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

        public void Clear()
        {
            _text.Clear();
            _currentLine.Clear();
            InitCurrentLine();
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

            // Number of leading spaces on current line, does not include sequence number and indicator
            int previousLineIndentLength = currentLine.Skip(_SequenceNumber.Length + 1).TakeWhile(c => c == ONE_SPACE).Count();

            _text.Append(currentLine);
            _currentLine.Clear();
            return previousLineIndentLength;
        }

        /// <summary>
        /// Append the given number of spaces at the beginning (after indicator) of the current line.
        /// This method expects that:
        /// - the current line is empty
        /// - the number of spaces does not exceed the maximum line length
        /// </summary>
        /// <param name="length">Number of leading spaces to add to current line.</param>
        public void AppendIndent(int length)
        {
            Debug.Assert(_currentLineIsEmpty);
            Debug.Assert(_currentLine.Length + length <= MAX_LINE_LENGTH);
            string indent = new string(ONE_SPACE, length);
            _currentLine.Append(indent);
        }

        /// <summary>
        /// Append the given word to the current line. The word is considered as non-breakable,
        /// meaning a new line will be created if the word does not fit on the current line.
        /// </summary>
        /// <param name="word">Cobol text to add on current line.</param>
        public void AppendWord(string word)
        {
            AppendText(word);
            _currentLineIsEmpty = false;
        }

        private void AppendText(string text)
        {
            bool addSeparator = !_currentLineIsEmpty;
            int separatorLength = addSeparator ? 1 : 0;
            if (_currentLine.Length + separatorLength + text.Length > MAX_LINE_LENGTH)
            {
                // Word is too long to fit on current line, create a new line and auto align
                int indent = AppendLine();
                AppendIndent(indent);
            }
            else if (addSeparator)
            {
                _currentLine.Append(ONE_SPACE);
            }

            _currentLine.Append(text);
        }

        /// <summary>
        /// Append the given literal after a DISPLAY keyword. This is a hack as this method does not
        /// handle literal splitting and continuation lines. Instead, the literal is broken at column 72
        /// and the concatenation is done at runtime by the DISPLAY statement itself.
        /// </summary>
        /// <param name="literalValue">Literal value to append, without delimiters.</param>
        /// <param name="delimiter">Desired delimiter, by default the method will use a single quote.</param>
        public void AppendLiteralForDisplay(string literalValue, char delimiter = '\'')
        {
            Debug.Assert(!_currentLineIsEmpty);
            Debug.Assert(_currentLine.ToString().EndsWith("DISPLAY"));

            bool addSeparator = true;
            int indent = _currentLine.Length - _SequenceNumber.Length; // If a new line is required, the text will be aligned with the beginning of the literal

            string text = literalValue;
            int remaining; // Remaining available chars on current line
            int addedCharsCount; // Number of extra chars required, 2 for the two delimiters or 3 when we also have to add the separator after DISPLAY keyword
            while (text.Length + (addedCharsCount = (addSeparator ? 3 : 2)) > (remaining = MAX_LINE_LENGTH - _currentLine.Length))
            {
                int partLength = remaining - addedCharsCount;
                if (partLength < 1)
                {
                    // Not enough remaining space on current line to add anything -> start a newline and continue
                    AppendLineAndIndent();
                    continue;
                }

                // Split literal
                string part = text.Substring(0, partLength);
                if (addSeparator)
                {
                    _currentLine.Append(ONE_SPACE);
                }

                // Append beginning
                _currentLine.Append(delimiter);
                _currentLine.Append(part);
                _currentLine.Append(delimiter);

                // Create new line and align
                AppendLineAndIndent();

                // Compute remaining text to append and continue
                text = text.Substring(part.Length);
            }

            // Append last part(this may be the full literal itself if it did not need to be split)
            if (text.Length > 0)
            {
                if (addSeparator)
                {
                    _currentLine.Append(ONE_SPACE);
                }

                _currentLine.Append(delimiter);
                _currentLine.Append(text);
                _currentLine.Append(delimiter);
                _currentLineIsEmpty = false;
            }

            void AppendLineAndIndent()
            {
                AppendLine();
                AppendIndent(indent);
                addSeparator = false;
            }
        }

        /// <summary>
        /// Break current line and initialize a new one.
        /// Return the current line indent length to allow caller to align text with previous line
        /// when starting writing on the newly created line.
        /// </summary>
        /// <returns>Current line (before break) indent length.</returns>
        public int AppendLine()
        {
            int previousLineIndentLength = FlushCurrentLine();
            _text.AppendLine();
            InitCurrentLine();
            return previousLineIndentLength;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="comment"></param>
        /// <exception cref="NotSupportedException"></exception>
        public void AppendCommentSingleLine(string comment)
        {
            comment ??= string.Empty;

            // 65 chars is the maximum single line comment length, this method does not support comment splitting
            Debug.Assert(comment.Length <= CobolFormatAreas.End_B - CobolFormatAreas.Begin_A + 1);

            if (_currentLineIsEmpty)
            {
                // The current line only contains the sequence number and the indicator and optionally some leading spaces
                _currentLine.Clear();
                _currentLine.Append(_SequenceNumber);
                _currentLine.Append(COMMENT_INDICATOR);
                _currentLine.Append(comment);
                AppendLine();
            }
            else
            {
                // Cannot append comment on non-empty line. Create a new one and retry.
                AppendLine();
                Debug.Assert(_currentLineIsEmpty);
                AppendCommentSingleLine(comment);
            }
        }

        /// <summary>
        /// Return full text of this builder.
        /// Note that the current line is being flushed.
        /// </summary>
        /// <returns>Full text of builder.</returns>
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

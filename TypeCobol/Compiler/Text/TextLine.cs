using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Immutable Cobol line for batch compilation.
    /// Text line loaded once from a file and never modified.
    /// </summary>
    public class TextLine : ITextLine
    {
        public TextLine(int lineIndex, int startOffset, string text)
        {
            LineIndex = lineIndex;
            StartOffset = startOffset;
            Text = text;
        }

        /// <summary>
        /// The first line has the index 0
        /// </summary>
        public int LineIndex { get; private set; }

        /// <summary>
        /// Offset of the first char of this line in the document 
        /// </summary>
        public int StartOffset { get; private set; }

        /// <summary>
        /// Text of the line, without the end of line delimiters
        /// </summary>
        public string Text { get; private set; }

        /// <summary>
        /// Part of the text of the line, from start index to end index (included)
        /// </summary>
        public string TextSegment(int startIndex, int endIndexInclusive)
        {
            return Text.Substring(startIndex, endIndexInclusive - startIndex + 1);
        }

        /// <summary>
        /// Number of characters in the line, end of line delimiters excluded
        /// </summary>
        public int Length
        {
            get
            {
                return Text.Length;
            }
        }
    }
}

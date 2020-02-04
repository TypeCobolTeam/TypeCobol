using System;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Enumeration of the standard text areas
    /// </summary>
    public enum TextAreaType
    {
        SequenceNumber,
        Indicator,
        Source,
        Comment,
        AreaA,
        AreaB
    }

    /// <summary>
    /// Portion of a text line with a specific meaning
    /// </summary>
    public class TextArea
    {
        public TextArea(TextAreaType type, int startIndex, int endIndex)
        {
            Type = type;
            StartIndex = startIndex;
            EndIndex = endIndex;
        }

        /// <summary>
        /// SequenceNumber, Indicator, Source, Comment
        /// </summary>
        public TextAreaType Type { get; private set; }

        /// <summary>
        /// Index of the first char of the area
        /// </summary>
        public int StartIndex { get; private set; }

        /// <summary>
        /// Index of the last char of the area
        /// </summary>
        public int EndIndex { get; private set; }

        /// <summary>
        /// True if EndIndex < StartIndex : no char in the area
        /// </summary>
        public bool IsEmpty
        {
            get { return EndIndex < StartIndex; }
        }
        
        public override string ToString()
        {
            return "[" + StartIndex + "," + EndIndex + "]";
        }
    }
}
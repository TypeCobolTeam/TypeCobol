namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Portion of a text line with a specific meaning
    /// </summary>
    public class TextArea
    {
        public TextArea(int startIndex, int endIndex)
        {
            StartIndex = startIndex;
            EndIndex = endIndex;
        }

        /// <summary>
        /// Index of the first char of the area
        /// </summary>
        public int StartIndex { get; }

        /// <summary>
        /// Index of the last char of the area
        /// </summary>
        public int EndIndex { get; }

        /// <summary>
        /// True if EndIndex < StartIndex : no char in the area
        /// </summary>
        public bool IsMissing => EndIndex < StartIndex;

        public override string ToString()
        {
            return "[" + StartIndex + "," + EndIndex + "]";
        }
    }
}

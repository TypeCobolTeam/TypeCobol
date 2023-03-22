using System;
using System.Text;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// A line in the document can be inserted, updated, or removed
    /// The whole document can be cleared of any content
    /// </summary>
    public enum TextChangeType
    {
        LineInserted,
        LineUpdated,
        LineRemoved,
        DocumentCleared
    }

    /// <summary>
    /// Text changes are tracked at the line level.
    /// This class models a simple change on one line of text.
    /// </summary>
    public class TextChange
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of change applied to the line</param>
        /// <param name="lineIndex">Index of the line which was changed</param>
        /// <param name="newLine">New line content after the update (null in case of a LineRemoved event)</param>
        internal TextChange(TextChangeType type, int lineIndex, ITextLine newLine)
        {
            Type = type;
            LineIndex = lineIndex;
            NewLine = newLine;
        }

        /// <summary>
        /// LineInserted, LineUpdated, or LineRemoved
        /// </summary>
        public TextChangeType Type { get; private set; }

        /// <summary>
        /// If a new line is inserted at index 2, the line previously stored at index 2 is now at index 3, and son on ...
        /// If the line at index 2 is removed, the previous previously stored at index 3 is now at index 2, and son on ...
        /// </summary>
        public int LineIndex { get; private set; }

        /// <summary>
        /// New line content after the update (null in case of a LineRemoved event)
        /// </summary>
        public ITextLine NewLine { get; private set; }

        public override string ToString() {
            var str = new StringBuilder();
            str.Append(Type).Append("@").Append(LineIndex).Append("\"").Append(NewLine == null ? String.Empty : NewLine.Text).Append("\"");
            return str.ToString();
        }
    }
}
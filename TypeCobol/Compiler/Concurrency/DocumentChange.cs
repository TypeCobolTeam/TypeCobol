#nullable enable

using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// A line in the document can be inserted, updated, or removed.
    /// The whole document can be cleared of any content.
    /// </summary>
    public enum DocumentChangeType
    {
        LineInserted,
        LineUpdated,
        LineRemoved,
        DocumentCleared
    }

    /// <summary>
    /// Document changes are tracked at the line level.
    /// This class models a simple change on one line of a document.
    /// </summary>
    public class DocumentChange<T>
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of change applied to the line</param>
        /// <param name="lineIndex">Index of the line which was changed</param>
        /// <param name="newLine">New line content after the update (null in case of a LineRemoved event)</param>
        public DocumentChange(DocumentChangeType type, int lineIndex, T? newLine)
        {
            Type = type;
            LineIndex = lineIndex;
            NewLine = newLine;
        }

        /// <summary>
        /// LineInserted, LineUpdated, LineRemoved, or DocumentCleared
        /// </summary>
        public DocumentChangeType Type { get; }

        /// <summary>
        /// If a new line is inserted at index 2, the line previously stored at index 2 is now at index 3, and so on ...
        /// If the line at index 2 is removed, the previous previously stored at index 3 is now at index 2, and so on ...
        /// Indexes start with the value 0.
        /// </summary>
        public int LineIndex { get; internal set; }

        /// <summary>
        /// New line content after the update (null in case of a LineRemoved event, or when computing an inverted change)
        /// </summary>
        public T? NewLine { get; internal set; }

        /// <summary>
        /// Reverts the change, useful when computing the sequence of changes
        /// to transform a new version to an older version of a document
        /// </summary>
        internal DocumentChange<T> Invert()
        {           
            if(Type == DocumentChangeType.LineInserted)
            {
                return new DocumentChange<T>(DocumentChangeType.LineRemoved, LineIndex, default(T));
            }
            else if(Type == DocumentChangeType.LineRemoved)
            {
                return new DocumentChange<T>(DocumentChangeType.LineInserted, LineIndex, default(T));
            }
            else // if(Type == DocumentChangeType.DocumentCleared || Type == DocumentChangeType.LineUpdated)
            {
                return this;
            }
        }
    }
}
using System;
using System.Collections.Generic;

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
    public class DocumentChange
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of change applied to the line</param>
        /// <param name="lineIndex">Index of the line which was changed</param>
        /// <param name="newLine">New line content after the update (null in case of a LineRemoved event)</param>
        public DocumentChange(DocumentChangeType type, int lineIndex, IDocumentLine newLine)
        {
            Type = type;
            LineIndex = lineIndex;
            NewLine = newLine;
        }

        /// <summary>
        /// LineInserted, LineUpdated, LineRemoved, or DocumentCleared
        /// </summary>
        public DocumentChangeType Type { get; private set; }

        /// <summary>
        /// If a new line is inserted at index 2, the line previously stored at index 2 is now at index 3, and so on ...
        /// If the line at index 2 is removed, the previous previously stored at index 3 is now at index 2, and so on ...
        /// Indexes start with the value 0.
        /// </summary>
        public int LineIndex { get; private set; }

        /// <summary>
        /// New line content after the update (null in case of a LineRemoved event, or when computing an inverted change)
        /// </summary>
        public IDocumentLine NewLine { get; private set; }

        /// <summary>
        /// Reverts the change, useful when computing the sequence of changes
        /// to transform a new version to an older version of a document
        /// </summary>
        internal DocumentChange Invert()
        {           
            if(Type == DocumentChangeType.LineInserted)
            {
                return new DocumentChange(DocumentChangeType.LineRemoved, LineIndex, null);
            }
            else if(Type == DocumentChangeType.LineRemoved)
            {
                return new DocumentChange(DocumentChangeType.LineInserted, LineIndex, null);
            }
            else // if(Type == DocumentChangeType.DocumentCleared || Type == DocumentChangeType.LineUpdated)
            {
                return this;
            }
        }
    }
}
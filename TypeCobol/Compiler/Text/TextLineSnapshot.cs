using System;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Snapshot of a text line in the source document
    /// </summary>
    public class TextLineSnapshot : ITextLine
    {
        public TextLineSnapshot(int lineIndex, string text, object lineTrackingReferenceInSourceDocument)
        {
            LineIndex = lineIndex;
            Text = text;
            LineTrackingReferenceInSourceDocument = lineTrackingReferenceInSourceDocument;
        }

        public TextLineSnapshot(ITextLine mutableTextLine)
        {
            LineIndex = mutableTextLine.LineIndex;
            Text = mutableTextLine.Text;
            LineTrackingReferenceInSourceDocument = mutableTextLine.LineTrackingReferenceInSourceDocument;
        }

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

        /// <summary>
        /// True if the implementation of the text line is read-only and can be used as a snapshot
        /// </summary>
        public bool IsReadOnly
        {
            get { return true; }
        }

        // Position of the text line in the source text document

        /// <summary>
        /// Index of this line when it first appeared in the document.
        /// </summary>
        public int LineIndex { get; set; }

        /// <summary>
        /// A text line instance can be reused simultaneously in different snapshots of the document
        /// (if it wasn't modified between two versions).
        /// You can NOT get a line number from an isolated text line, because this line instance can
        /// have different positions in two different snapshots of the document (if other lines were 
        /// inserted or removed before).
        /// 
        /// The line number is only defined :
        /// 
        /// 1. In a specific snapshot of the document :
        /// - pass the ITextLine object to the IndexOf method on the list of lines in a document snapshot 
        ///   (WARNING : expensive O(n) operation !)
        /// 
        /// 2. In the live text document (for example a text editor accessed in the specific thread where it lives) :
        /// - pass the property LineTrackingReferenceInSourceDocument to a dedicated method of the text source 
        ///   (much less expensive O(log n) operation)
        /// 
        /// This property returns an opaque reference to a line tracking object from the live text document,
        /// which will enable an efficient retrieval of the line number for this line in the document.
        /// </summary>
        public object LineTrackingReferenceInSourceDocument { get; private set; }
    }
}

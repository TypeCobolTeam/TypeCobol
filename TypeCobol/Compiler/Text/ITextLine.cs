using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Text
{    
    /// <summary>
    /// Interface enabling the integration of the Cobol compiler with any kind of text editor.
    /// </summary>
    public interface ITextLine
    {
        /// <summary>
        /// Text of the line, without the end of line delimiters
        /// </summary>
        string Text { get; }

        /// <summary>
        /// Part of the text of the line, from start index to end index (included)
        /// </summary>
        string TextSegment(int startIndex, int endIndexInclusive);

        /// <summary>
        /// Number of characters in the line, end of line delimiters excluded
        /// </summary>
        int Length { get; }

        /// <summary>
        /// True if the implementation of the text line is read-only and can be used as a snapshot
        /// </summary>
        bool IsReadOnly { get; }

        // Position of the text line in the source text document

        /// <summary>
        /// Index of this line when it first appeared in the document.
        /// </summary>
        int LineIndex { get; set; }

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
        object LineTrackingReferenceInSourceDocument { get; }
    }
}

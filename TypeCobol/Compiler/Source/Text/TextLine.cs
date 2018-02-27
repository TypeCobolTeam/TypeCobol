using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Source.Text
{
    /// <summary>
    /// A Mutable TextLine object that is a SourceLine attached to a SourceDocument.
    /// </summary>
    public class TextLine : SourceDocument.SourceLine, ITextLine
    {
        public TextLine(Position start, Position end, TextDocument document) : base(start, end)
        {
            Document = document;
        }

        /// <summary>
        /// The owner source document
        /// </summary>
        public TextDocument Document
        {
            get;
            private set;
        }
        public bool IsReadOnly => false;

        public int Length => To - From;

        public int LineIndex
        {
            get
            {
                return Document.GetLineIndex(From);
            }

            set
            {
                throw new NotImplementedException();
            }
        }

        public object LineTrackingReferenceInSourceDocument => Document;

        public string Text => (Document as SourceDocument).Source.GetTextAt(From, To);

        public string TextSegment(int startIndex, int endIndexInclusive)
        {
            return Text.Substring(startIndex, endIndexInclusive - startIndex + 1);
        }
    }
}

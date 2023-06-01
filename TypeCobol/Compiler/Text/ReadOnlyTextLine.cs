#nullable enable

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Read-only text line for batch compilation.
    /// Text line loaded once from a file and never modified.
    /// </summary>
    public class ReadOnlyTextLine : TextLineSnapshot
    {
        public ReadOnlyTextLine(int lineIndex, int startOffset, string text, object? lineTrackingReferenceInSourceDocument) :
            base(lineIndex, text, lineTrackingReferenceInSourceDocument)
        {
            StartOffset = startOffset;
        }

       

        /// <summary>
        /// Offset of the first char of this line in the document 
        /// </summary>
        public int StartOffset { get; private set; }
    }
}

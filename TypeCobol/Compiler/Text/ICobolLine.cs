namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Partition of a COBOL text line into reference format areas
    /// </summary>
    public interface ICobolLine : ITextLine
    {
        /// <summary>
        /// Cobol text line type : Source, Debug, Comment or Continuation
        /// </summary>
        TextLineType Type { get; set; }

        /// <summary>
        /// Sequence number area : Columns 1 through 6
        /// </summary>
        TextArea SequenceNumber { get; }

        /// <summary>
        /// Sequence number text : Columns 1 through 6
        /// </summary>
        string SequenceNumberText { get; }

        /// <summary>
        /// Indicator area : Column 7
        /// </summary>
        TextArea Indicator { get; }

        /// <summary>
        /// Indicator char : Column 7
        /// </summary>
        char IndicatorChar { get; }

        /// <summary>
        /// Area A : Columns 8 through 11 
        /// Area B : Columns 12 through 72 
        /// </summary>
        TextArea Source { get; }

        /// <summary>
        /// Area A text : Columns 8 through 11 
        /// Area B text : Columns 12 through 72 
        /// </summary>
        string SourceText { get; }

        /// <summary>
        /// Comment area : Columns 73 through 80+
        /// </summary>
        TextArea Comment { get; }

        /// <summary>
        /// Comment text : Columns 73 through 80+
        /// </summary>
        string CommentText { get; }
    }
}
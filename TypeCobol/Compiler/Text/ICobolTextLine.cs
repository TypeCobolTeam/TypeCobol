#nullable enable

using TypeCobol.Compiler.Concurrency;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Partition of a COBOL text line into reference format areas
    /// </summary>
    public interface ICobolTextLine : ITextLine
    {
        /// <summary>
        /// Columns format
        /// </summary>
        ColumnsLayout ColumnsLayout { get; }

        /// <summary>
        /// Cobol text line type : Source, Debug, Comment or Continuation
        /// </summary>
        CobolTextLineType Type { get; }

        /// <summary>
        /// Sequence number text : Columns 1 through 6
        /// </summary>
        string? SequenceNumberText { get; }

        /// <summary>
        /// Indicator area : Column 7
        /// </summary>
        TextArea Indicator { get; }

        /// <summary>
        /// Indicator char : Column 7
        /// </summary>
        char IndicatorChar { get; } // TODO turn into char? to respect FreeTextFormat

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
        /// Comment text : Columns 73 through 80+
        /// </summary>
        string? CommentText { get; }

        // --- Incremental compilation process ---

        /// <summary>
        /// Indicates which compiler step last updated the properties of this line
        /// </summary>
        CompilationStep CompilationStep { get; set; }

        /// <summary>
        /// A line is frozen after the completion of each compiler step to enable reliable snapshots.
        /// If we need to update the properties of the line later, a new line must be allocated.
        /// This method returns true if the line can be updated in place, false if a new copy of the line must be allocated.
        /// </summary>
        bool CanStillBeUpdatedBy(CompilationStep updatingStep);
    }
}
using System;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Characteristic Cobol Format Areas.
    /// This enumeration gives column positions or ranges of characteristic areas.
    /// </summary>
    public enum CobolFormatAreas : int
    {
        // Columns 1-6 : Sequence number
        BeginNumber = 1,
        EndNumber = 6,
        // Columns 7 : Indicator
        Indicator = 7,
        // Columns 8-11 : Text Area A 
        Begin_A = 8,
        End_A = 11,
        // Columns 12-12 : Text Area B
        Begin_B = 12,
        End_B = 72,
        // Columns 73 : Comment
        Comment = 73,
    };
}


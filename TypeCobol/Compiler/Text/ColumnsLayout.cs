using System;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Enumeration of standard formats for a Cobol text line
    /// </summary>
    public enum ColumnsLayout
    {
        /// <summary>
        /// Fixed-form reference format
        /// Columns 1-6 : Sequence number
        /// Columns 7 : Indicator
        /// Columns 8-72 : Text Area A and Area B
        /// Columns 73-> : Comment
        /// </summary>
        CobolReferenceFormat,
        /// <summary>
        /// Like CobolReferenceFormat but with unlimited line length.
        /// This column layout should only be used by continuation line
        /// </summary>
        CobolReferenceFormatUnlimitedLength,
        /// <summary>
        /// Free-form format
        /// There is not limit on the size a source line.
        /// The first seven characters of each line are considered part of the normal source line and may contain COBOL source code.
        /// Column 1 takes the role of the indicator area as follows:
        ///    * comment line 
        ///    / comment line starting on a new page in the listing file 
        ///    - continuation line
        ///    D or d followed by space, debugging line 
        ///    any other character normal source line.
        /// There is no fixed right margin, but floating comment indicators : *>.
        /// </summary>
        FreeTextFormat
    }
}

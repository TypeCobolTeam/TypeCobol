using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Text
{    
    /// <summary>
    /// Interface enabling the integration of the Cobol compiler with any kind of text editor.
    /// Each line has an index to describe its position in the document.
    /// </summary>
    public interface ITextLine
    {
        /// <summary>
        /// The first line has the index 0
        /// </summary>
        int LineIndex { get; }

        /// <summary>
        /// Offset of the first char of this line in the document 
        /// </summary>
        int StartOffset { get; }

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
    }
}

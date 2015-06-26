using System;
using System.Text;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Informations on the source file on disk (or the buffer in memory) from which a text document was loaded
    /// </summary>
    public class TextSourceInfo
    {
        public TextSourceInfo(string name, Encoding encodingForHexadecimalAlphanumericLiterals, ColumnsLayout columnsLayout)
        {
            Name = name;
            EncodingForHexadecimalAlphanumericLiterals = encodingForHexadecimalAlphanumericLiterals;
            ColumnsLayout = columnsLayout;
        }

        /// <summary>
        /// Name of the source file on disk (or the buffer in memory) from which a text document was loaded.
        /// Could be null if no name has been set for an in-memory buffer.
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Character set used to encode the hexadecimal alphanumeric literals in the text document
        /// </summary>
        public Encoding EncodingForHexadecimalAlphanumericLiterals { get; private set; }

        /// <summary>
        /// Format of the text document lines : Cobol reference format on 72 columns, or free text format
        /// </summary>
        public ColumnsLayout ColumnsLayout { get; private set; }
    }
}

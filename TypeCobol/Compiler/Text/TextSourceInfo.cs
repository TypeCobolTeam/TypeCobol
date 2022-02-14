using System.Text;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Information on the source file on disk, or the buffer in memory, from which a text document was loaded
    /// </summary>
    public class TextSourceInfo
    {
        public TextSourceInfo(string name, Encoding encodingForAlphanumericLiterals, ColumnsLayout columnsLayout, bool isCopy)
        {
            Name = name;
            EncodingForAlphanumericLiterals = encodingForAlphanumericLiterals;
            ColumnsLayout = columnsLayout;
            IsCopy = isCopy;
        }

        /// <summary>
        /// Name of the source file on disk (or the buffer in memory) from which a text document was loaded.
        /// Could be null if no name has been set for an in-memory buffer.
        /// </summary>
        public string Name { get; }

        /// <summary>
        /// Character set used to encode the hexadecimal alphanumeric literals in the text document
        /// </summary>
        public Encoding EncodingForAlphanumericLiterals { get; }

        /// <summary>
        /// Format of the text document lines : Cobol reference format on 72 columns, or free text format
        /// </summary>
        public ColumnsLayout ColumnsLayout { get; }

        /// <summary>
        /// Nature of expected file content: True for copybook, False for program.
        /// </summary>
        public bool IsCopy { get; }
    }
}

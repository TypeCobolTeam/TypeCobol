using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The FILE SECTION defines the structure of data files.
    /// </summary>
    public class FileSectionHeader : CodeElement
    {
        public FileSectionHeader() : base(CodeElementType.FileSectionHeader)
        { }
    }
}

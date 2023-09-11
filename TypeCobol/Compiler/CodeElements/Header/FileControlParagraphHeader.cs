using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The FILE-CONTROL paragraph associates each file in the COBOL program with
    /// an external data set, and specifies file organization, access mode, and other
    /// information.
    /// </summary>
    public class FileControlParagraphHeader : CodeElement
    {
        public FileControlParagraphHeader() : base(CodeElementType.FileControlParagraphHeader)
        { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.AreaA;
    }
}

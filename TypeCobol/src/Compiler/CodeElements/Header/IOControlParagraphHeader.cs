using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The I-O-CONTROL paragraph of the input-output section specifies when checkpoints are to be taken 
    /// and the storage areas to be shared by different files. 
    /// </summary>
    public class IOControlParagraphHeader : CodeElement
    {
        public IOControlParagraphHeader() : base(CodeElementType.IOControlParagraphHeader)
        { }
    }
}

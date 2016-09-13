using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The input-output section of the ENVIRONMENT DIVISION contains
    /// FILE-CONTROL paragraph and I-O-CONTROL paragraph.
    /// </summary>
    public class InputOutputSectionHeader : CodeElement
    {
        public InputOutputSectionHeader() : base(CodeElementType.InputOutputSectionHeader)
        { }
    }
}

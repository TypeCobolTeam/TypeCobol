using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The WORKING-STORAGE SECTION describes data records that are not part of data files but are developed and processed by a program or method. 
    /// The WORKING-STORAGE SECTION also describes data items whose values are assigned in the source program or method and do not change during execution of the object program.
    /// </summary>
    public class WorkingStorageSectionHeader : CodeElement
    {
        public WorkingStorageSectionHeader() : base(CodeElementType.WorkingStorageSectionHeader)
        { }
    }
}

using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The LOCAL-STORAGE SECTION defines storage that is allocated and freed on a per-invocation basis.
    /// </summary>
    public class LocalStorageSectionHeader : CodeElement
    {
        public LocalStorageSectionHeader() : base(CodeElementType.LocalStorageSectionHeader)
        { }
    }
}

using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The LINKAGE SECTION describes data made available from another program or method.
    /// </summary>
    public class LinkageSectionHeader : CodeElement
    {
        public LinkageSectionHeader() : base(CodeElementType.LinkageSectionHeader)
        { }
    }
}

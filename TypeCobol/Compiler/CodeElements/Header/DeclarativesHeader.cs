using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Declaratives provide one or more special-purpose sections that are executed when
    /// an exceptional condition occurs.
    /// </summary>
    public class DeclarativesHeader : CodeElement
    {
        public DeclarativesHeader() : base(CodeElementType.DeclarativesHeader)
        { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.Unspecified;
    }
}

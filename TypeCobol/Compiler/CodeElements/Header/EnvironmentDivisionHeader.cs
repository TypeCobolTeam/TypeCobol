using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Environment division
    /// </summary>
    public class EnvironmentDivisionHeader : CodeElement
    {
        public EnvironmentDivisionHeader() : base(CodeElementType.EnvironmentDivisionHeader)
        { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.AreaA;
    }
}

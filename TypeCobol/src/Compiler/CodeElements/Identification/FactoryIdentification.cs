using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Factory IDENTIFICATION DIVISION
    /// A factory IDENTIFICATION DIVISION contains only a factory paragraph
    /// header.
    /// </summary>
    public class FactoryIdentification : CodeElement
    {
        public FactoryIdentification() : base(CodeElementType.FactoryIdentification)
        { }
    }
}

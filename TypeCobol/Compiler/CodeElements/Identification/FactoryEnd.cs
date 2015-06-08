using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The factory IDENTIFICATION DIVISION introduces the factory definition, which
    /// is the portion of a class definition that defines the factory object of the class.
    /// </summary>
    public class FactoryEnd : CodeElement
    {
        public FactoryEnd() : base(CodeElementType.FactoryEnd)
        { }
    }
}

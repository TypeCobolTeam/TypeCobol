using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Object IDENTIFICATION DIVISION
    /// An object IDENTIFICATION DIVISION contains only an object paragraph
    /// header.
    /// The object IDENTIFICATION DIVISION introduces the object definition, which is
    /// the portion of a class definition that defines the instance objects of the class.
    /// </summary>
    public class ObjectIdentification : CodeElement
    {
        public ObjectIdentification() : base(CodeElementType.ObjectIdentification)
        { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.AreaA;
    }
}

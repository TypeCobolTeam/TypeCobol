using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The object IDENTIFICATION DIVISION introduces the object definition, which is
    /// the portion of a class definition that defines the instance objects of the class.
    /// </summary>
    public class ObjectEnd : CodeElementEnd
    {
        public ObjectEnd() : base(CodeElementType.ObjectEnd)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
    }
}

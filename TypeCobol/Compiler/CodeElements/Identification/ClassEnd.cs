using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The end of a COBOL class definition is indicated by the END CLASS marker.
    /// </summary>
    public class ClassEnd : CodeElement
    {
        public ClassEnd() : base(CodeElementType.ClassEnd)
        { }

        /// <summary>
        /// class-name
        /// A user-defined word that identifies the class.
        /// </summary>
        public ClassName ClassName { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return base.ToString() + "{ClassName=" + ClassName + "}";
        }
    }
}

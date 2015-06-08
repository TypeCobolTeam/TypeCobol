using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The METHOD-ID paragraph specifies the name by which a method is known and
    /// assigns selected attributes to that method.
    /// </summary>
    public class MethodEnd : CodeElement
    {
        public MethodEnd() : base(CodeElementType.MethodEnd)
        { }

        /// <summary>
        /// method-name
        /// An alphanumeric literal or national literal that contains the name of the
        /// method.
        /// </summary>
        public MethodName MethodName { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return base.ToString() + "{MethodName=" + MethodName + "}";
        }
    }
}

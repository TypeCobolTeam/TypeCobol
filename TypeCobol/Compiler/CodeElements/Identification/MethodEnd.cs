using System;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The METHOD-ID paragraph specifies the name by which a method is known and
    /// assigns selected attributes to that method.
    /// </summary>
    public class MethodEnd : CodeElementEnd
    {
        public MethodEnd() : base(CodeElementType.MethodEnd)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
        /// <summary>
        /// method-name
        /// An alphanumeric literal or national literal that contains the name of the
        /// method.
        /// </summary>
        public SymbolReference MethodName { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- MethodName = " + MethodName);
            return sb.ToString();
        }
    }
}

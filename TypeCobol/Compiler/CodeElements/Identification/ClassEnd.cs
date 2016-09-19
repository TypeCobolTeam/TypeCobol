using System;
using System.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The end of a COBOL class definition is indicated by the END CLASS marker.
    /// </summary>
    public class ClassEnd : CodeElementEnd
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
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- ClassName = " + ClassName);
            return sb.ToString();
        }
    }
}

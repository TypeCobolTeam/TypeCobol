using System;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Method IDENTIFICATION DIVISION
    /// For a method, the first paragraph of the IDENTIFICATION DIVISION
    /// must be the METHOD-ID paragraph. The other paragraphs are optional
    /// and can appear in any order.
    /// The METHOD-ID paragraph specifies the name by which a method is known and
    /// assigns selected attributes to that method.
    /// </summary>
    public class MethodIdentification : CodeElement
    {
        public MethodIdentification() : base(CodeElementType.MethodIdentification)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
        /// <summary>
        /// method-name
        /// An alphanumeric literal or national literal that contains the name of the
        /// method. The name must conform to the rules of formation for a Java
        /// method name. Method names are used directly, without translation. The
        /// method name is processed in a case-sensitive manner.
        /// </summary>
        public SymbolDefinition MethodName { get; set; }

        /// <summary>
        /// Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
        /// The optional paragraphs are: 
        /// AUTHOR, INSTALLATION, DATE-WRITTEN, DATE-COMPILED, SECURITY
        /// </summary>
        public AuthoringProperties AuthoringProperties { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- MethodName = " + MethodName);
            if (AuthoringProperties != null)
            {
                sb.Append(AuthoringProperties);
            }
            return sb.ToString();
        }
    }
}

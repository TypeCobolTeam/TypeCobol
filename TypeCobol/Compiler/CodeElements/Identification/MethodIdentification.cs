using System;

namespace TypeCobol.Compiler.CodeElements
{
    public class MethodIdentification : CodeElement
    {
        public MethodIdentification() : base(CodeElementType.MethodIdentification)
        {
            AuthoringProperties = new AuthoringProperties();
        }

        /// <summary>
        /// method-name
        /// An alphanumeric literal or national literal that contains the name of the
        /// method. The name must conform to the rules of formation for a Java
        /// method name. Method names are used directly, without translation. The
        /// method name is processed in a case-sensitive manner.
        /// </summary>
        public MethodName Name { get; set; }

        /// <summary>
        /// Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
        /// The optional paragraphs are: 
        /// AUTHOR, INSTALLATION, DATE-WRITTEN, DATE-COMPILED, SECURITY
        /// </summary>
        public AuthoringProperties AuthoringProperties { get; set; }
    }
}

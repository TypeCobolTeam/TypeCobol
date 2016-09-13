﻿using System;
using System.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
    /// The optional paragraphs are:
    /// AUTHOR, INSTALLATION, DATE-WRITTEN, DATE-COMPILED, SECURITY
    /// </summary>
    public class AuthoringProperties
    {
        /// <summary>
        /// AUTHOR
        /// Name of the author of the program. 
        /// </summary>
        public SyntaxProperty<string> Author { get; set; }

        /// <summary>
        /// INSTALLATION
        /// Name of the company or location.
        /// </summary>
        public SyntaxProperty<string> Installation { get; set; }

        /// <summary>
        /// DATE-WRITTEN
        /// Date the program was written.
        /// </summary>
        public SyntaxProperty<string> DateWritten { get; set; }

        /// <summary>
        /// DATE-COMPILED
        /// The DATE-COMPILED paragraph provides the compilation date in the
        /// source listing. If a comment-entry is specified, the entire entry is replaced
        /// with the current date, even if the entry spans lines. If the comment entry is
        /// omitted, the compiler adds the current date to the line on which
        /// DATE-COMPILED is printed. For example:
        /// DATE-COMPILED. 06/30/10.
        /// </summary>
        public SyntaxProperty<string> DateCompiled { get; set; }

        /// <summary>
        /// SECURITY
        /// Level of confidentiality of the program.
        /// </summary>
        public SyntaxProperty<string> Security { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            if(Author != null)
            {
                sb.AppendLine("- AUTHOR = " + Author.Value.Replace("\r", " ").Replace("\n", " "));
            }
            if (DateCompiled != null)
            {
                sb.AppendLine("- DATE-COMPILED = " + DateCompiled.Value.Replace("\r", " ").Replace("\n", " "));
            }
            if (DateWritten != null)
            {
                sb.AppendLine("- DATE-WRITTEN = " + DateWritten.Value.Replace("\r", " ").Replace("\n", " "));
            }
            if (Installation != null)
            {
                sb.AppendLine("- INSTALLATION = " + Installation.Value.Replace("\r", " ").Replace("\n", " "));
            }
            if (Security != null)
            {
                sb.AppendLine("- SECURITY = " + Security.Value.Replace("\r", " ").Replace("\n", " "));
            }
            return sb.ToString();
        }
    }
}

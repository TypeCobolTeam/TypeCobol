using System;
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
        public string Author { get; set; }

        /// <summary>
        /// INSTALLATION
        /// Name of the company or location.
        /// </summary>
        public string Installation { get; set; }

        /// <summary>
        /// DATE-WRITTEN
        /// Date the program was written.
        /// </summary>
        public string DateWritten { get; set; }

        /// <summary>
        /// DATE-COMPILED
        /// The DATE-COMPILED paragraph provides the compilation date in the
        /// source listing. If a comment-entry is specified, the entire entry is replaced
        /// with the current date, even if the entry spans lines. If the comment entry is
        /// omitted, the compiler adds the current date to the line on which
        /// DATE-COMPILED is printed. For example:
        /// DATE-COMPILED. 06/30/10.
        /// </summary>
        public string DateCompiled { get; set; }

        /// <summary>
        /// SECURITY
        /// Level of confidentiality of the program.
        /// </summary>
        public string Security { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            if(!String.IsNullOrEmpty(Author))
            {
                sb.AppendLine("- AUTHOR = " + Author.Replace("\r", " ").Replace("\n", " "));
            }
            if (!String.IsNullOrEmpty(DateCompiled))
            {
                sb.AppendLine("- DATE-COMPILED = " + DateCompiled.Replace("\r", " ").Replace("\n", " "));
            }
            if (!String.IsNullOrEmpty(DateWritten))
            {
                sb.AppendLine("- DATE-WRITTEN = " + DateWritten.Replace("\r", " ").Replace("\n", " "));
            }
            if (!String.IsNullOrEmpty(Installation))
            {
                sb.AppendLine("- INSTALLATION = " + Installation.Replace("\r", " ").Replace("\n", " "));
            }
            if (!String.IsNullOrEmpty(Security))
            {
                sb.AppendLine("- SECURITY = " + Security.Replace("\r", " ").Replace("\n", " "));
            }
            return sb.ToString();
        }
    }
}

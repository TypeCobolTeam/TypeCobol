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
        public AlphanumericValue[] Author { get; set; }

        /// <summary>
        /// INSTALLATION
        /// Name of the company or location.
        /// </summary>
        public AlphanumericValue[] Installation { get; set; }

        /// <summary>
        /// DATE-WRITTEN
        /// Date the program was written.
        /// </summary>
        public AlphanumericValue[] DateWritten { get; set; }

        /// <summary>
        /// DATE-COMPILED
        /// The DATE-COMPILED paragraph provides the compilation date in the
        /// source listing. If a comment-entry is specified, the entire entry is replaced
        /// with the current date, even if the entry spans lines. If the comment entry is
        /// omitted, the compiler adds the current date to the line on which
        /// DATE-COMPILED is printed. For example:
        /// DATE-COMPILED. 06/30/10.
        /// </summary>
        public AlphanumericValue[] DateCompiled { get; set; }

        /// <summary>
        /// SECURITY
        /// Level of confidentiality of the program.
        /// </summary>
        public AlphanumericValue[] Security { get; set; }

        public AuthoringProperties() {
			Author = new AlphanumericValue[0];
			Installation = new AlphanumericValue[0];
			DateWritten = new AlphanumericValue[0];
			DateCompiled = new AlphanumericValue[0];
			Security = new AlphanumericValue[0];
		}

        /// <summary>Print the text corresponding to one of the authoring properties</summary>
        private void DumpPropertyValue(StringBuilder str, string label, AlphanumericValue[] entries) {
			if (entries == null || entries.Length < 1) return;
			str.Append(label);
			foreach(var entry in entries) str.Append(entry.Value).Append(' ');
			if (entries.Length > 0) str.Length -= 1;
			str.AppendLine();
		}
        public override string ToString() {
			var str = new StringBuilder();
			DumpPropertyValue(str, "- AUTHOR = ",        Author);
			DumpPropertyValue(str, "- DATE-COMPILED = ", DateCompiled);
			DumpPropertyValue(str, "- DATE-WRITTEN = ",  DateWritten);
			DumpPropertyValue(str, "- INSTALLATION = ",  Installation);
			DumpPropertyValue(str, "- SECURITY = ",      Security);
			return str.ToString();
		}
    }
}

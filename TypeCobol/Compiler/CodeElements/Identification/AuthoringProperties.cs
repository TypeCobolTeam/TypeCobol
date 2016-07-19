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


		/// <summary>
		/// Get the text corresponding to one of the authoring properties
		/// </summary>
		public string GetPropertyValue(AlphanumericValue[] commentEntries)
		{
			StringBuilder sb = new StringBuilder();
			if(commentEntries != null)
			{
				for(int i = 0; i < commentEntries.Length; i++)
				{
					if (i > 0) sb.Append(' ');
					sb.Append(commentEntries[i].Value);
				}
			}
			return sb.ToString();
		}

		/// <summary>
		/// Debug string
		/// </summary>
		public override string ToString()
		{
			StringBuilder sb = new StringBuilder();
			if(Author != null)
			{
				sb.AppendLine("- AUTHOR = " + GetPropertyValue(Author));
			}
			if (DateCompiled != null)
			{
				sb.AppendLine("- DATE-COMPILED = " + GetPropertyValue(DateCompiled));
			}
			if (DateWritten != null)
			{
				sb.AppendLine("- DATE-WRITTEN = " + GetPropertyValue(DateWritten));
			}
			if (Installation != null)
			{
				sb.AppendLine("- INSTALLATION = " + GetPropertyValue(Installation));
			}
			if (Security != null)
			{
				sb.AppendLine("- SECURITY = " + GetPropertyValue(Security));
			}
			return sb.ToString();
		}
	}
}

using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
	public interface Returning {
		CallTargetParameter ReturningParameter { get; set; }
	}

	/// <summary>
	/// The program procedure division consists of optional declaratives, and
	/// procedures that contain sections, paragraphs, sentences, and statements.
	/// </summary>
	public class ProcedureDivisionHeader : StatementElement, Returning, IFormalizedCommentable
	{
	    public ProcedureDivisionHeader(FormalizedCommentDocumentation formalizedCommentDocumentation = null) : base(
	        CodeElementType.ProcedureDivisionHeader, StatementType.ProcedureDivisionHeader)
	    {
	        this.FormalizedCommentDocumentation = formalizedCommentDocumentation;
        }
        public override TextAreaType StartingArea => TextAreaType.AreaA;

		/// <summary>
		/// The USING phrase specifies the parameters that a program or method receives
		/// when the program is called or the method is invoked.
		/// Each USING identifier must be defined as a level-01 or level-77 item in the
		/// LINKAGE SECTION of the called subprogram or invoked method.
		/// The argument receiving mode can be : BY REFERENCE or BY VALUE
		/// </summary>
		public IList<CallTargetParameter> UsingParameters { get; set; }

		/// <summary>
		/// The RETURNING phrase specifies a data item that is to receive the program or
		/// method result.
		/// The RETURNING data item must be a level-01 or level-77 item in the LINKAGE SECTION.
		/// The RETURNING data item is an output-only parameter.
		/// </summary>
		public CallTargetParameter ReturningParameter { get; set; }
	    public FormalizedCommentDocumentation FormalizedCommentDocumentation { get; set; }

        public override string ToString() {
			if (UsingParameters == null && ReturningParameter == null) return base.ToString();

			var str = new StringBuilder(base.ToString());
			if (UsingParameters != null) {
				str.Append("- InputParameters =");
				foreach (var inputParam in UsingParameters) {
					str.Append(' ');
					if (inputParam.SharingMode != null) {
						str.Append(inputParam.SharingMode);
						str.Append(':');
					}
					var named = inputParam.StorageArea;
					if (named == null) str.Append('?');
					else str.Append(named.SymbolReference?.Name);
				}
				str.AppendLine();
			}
			if (ReturningParameter != null) {
				str.Append("- ReturningDataName = ");
				var named = ReturningParameter.StorageArea;
				if (named == null) str.Append('?');
				else str.Append(named.SymbolReference?.Name);
				str.AppendLine();
			}
			return str.ToString();
		}
	}
}

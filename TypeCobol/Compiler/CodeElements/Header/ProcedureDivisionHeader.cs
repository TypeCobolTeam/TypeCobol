using System;
using System.Collections.Generic;
using System.Text;

namespace TypeCobol.Compiler.CodeElements
{
	public interface Returning {
		ReceivingStorageArea ReturningParameter { get; set; }
	}

	/// <summary>
	/// The program procedure division consists of optional declaratives, and
	/// procedures that contain sections, paragraphs, sentences, and statements.
	/// </summary>
	public class ProcedureDivisionHeader : StatementElement, Returning {
		public ProcedureDivisionHeader() : base(CodeElementType.ProcedureDivisionHeader, StatementType.ProcedureDivisionHeader) { }

		/// <summary>
		/// The USING phrase specifies the parameters that a program or method receives
		/// when the program is called or the method is invoked.
		/// Each USING identifier must be defined as a level-01 or level-77 item in the
		/// LINKAGE SECTION of the called subprogram or invoked method.
		/// The argument receiving mode can be : BY REFERENCE or BY VALUE
		/// </summary>
		public IList<InputParameter> UsingParameters { get; set; }

		/// <summary>
		/// The RETURNING phrase specifies a data item that is to receive the program or
		/// method result.
		/// The RETURNING data item must be a level-01 or level-77 item in the LINKAGE SECTION.
		/// The RETURNING data item is an output-only parameter.
		/// </summary>
		public ReceivingStorageArea ReturningParameter { get; set; }

		public override string ToString() {
			if (UsingParameters == null && ReturningParameter == null) return base.ToString();

			var str = new StringBuilder(base.ToString());
			if (UsingParameters != null) {
				str.Append("- InputParameters =");
				foreach (InputParameter inputParam in UsingParameters) {
					str.Append(' ');
					if (inputParam.ReceivingMode != null) {
						str.Append(inputParam.ReceivingMode);
						str.Append(':');
					}
					var named = inputParam.ReceivingStorageArea.StorageArea;
					if (named == null) str.Append('?');
					else str.Append(named.SymbolReference.Name);
				}
				str.AppendLine();
			}
			if (ReturningParameter != null) {
				str.Append("- ReturningDataName = ");
				var named = ReturningParameter.StorageArea;
				if (named == null) str.Append('?');
				else str.Append(named.SymbolReference.Name);
				str.AppendLine();
			}
			return str.ToString();
		}
	}

	/// <summary>
	/// The USING phrase specifies the parameters that a program or method receives
	/// when the program is called or the method is invoked.
	/// Each USING identifier must be defined as a level-01 or level-77 item in the
	/// LINKAGE SECTION of the called subprogram or invoked method.
	/// </summary>
	public class InputParameter {
		/// <summary>
		/// Argument receiving mode : BY REFERENCE or BY VALUE
		/// </summary>
		public SyntaxProperty<ParameterSharingMode> ReceivingMode { get; set; }
		/// <summary>
		/// Each USING identifier must be defined as a level-01 or level-77 item in the
		/// LINKAGE SECTION of the called subprogram or invoked method.
		/// </summary>
		public ReceivingStorageArea ReceivingStorageArea { get; set; }
	}	
}

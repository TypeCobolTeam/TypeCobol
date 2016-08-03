using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements {

	public class FunctionDeclarationProfile: CodeElement {

		/// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
		public IList<InputParameter> InputParameters  { get; internal set; }
		/// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
		public IList<DataName> OutputParameters { get; internal set; }
		/// <summary>INOUT datanames, always passed BY REFERENCE.</summary>
		public IList<DataName> InoutParameters { get; internal set; }
		/// <summary>RETURNING dataname.</summary>
		public DataName ReturningParameter { get; set; }

		public FunctionDeclarationProfile(): base(CodeElementType.ProcedureDivisionHeader) {
			InputParameters = new List<InputParameter>();
			OutputParameters = new List<DataName>();
			InoutParameters = new List<DataName>();
		}

		public FunctionDeclarationProfile(ProcedureDivisionHeader other): this() {
			this.ReturningParameter = other.ReturningParameter;
			this.ConsumedTokens = other.ConsumedTokens;
		}
	}
}

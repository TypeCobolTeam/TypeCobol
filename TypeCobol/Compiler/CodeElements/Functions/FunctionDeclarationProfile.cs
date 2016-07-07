using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements {

	public class FunctionDeclarationProfile: CodeElement {

		/// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
		public IList<InputParameter> InputParameters  { get; internal set; }
		/// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
		public IList<DataName> OutputParameters { get; internal set; }

		public FunctionDeclarationProfile(): base(CodeElementType.ProcedureDivisionHeader) {
			InputParameters = new List<InputParameter>();
			OutputParameters = new List<DataName>();
		}

		public FunctionDeclarationProfile(ProcedureDivisionHeader other): this() {
			if (other.ReturningDataName != null) OutputParameters.Add(other.ReturningDataName);
			this.ConsumedTokens = other.ConsumedTokens;
		}
	}
}

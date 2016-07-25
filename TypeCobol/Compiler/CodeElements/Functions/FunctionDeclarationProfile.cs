using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements {

	public class FunctionDeclarationProfile: StatementElement, Returning {
		public FunctionDeclarationProfile()
			: base(CodeElementType.FunctionDeclarationHeader, StatementType.ProcedureDivisionHeader)
		{
			InputParameters = new List<InputParameter>();
			OutputParameters = new List<ReceivingStorageArea>();
		}

		public FunctionDeclarationProfile(ProcedureDivisionHeader other): this() {
			this.InputParameters = other.InputParameters;
			this.ReturningParameter = other.ReturningParameter;
			this.ConsumedTokens = other.ConsumedTokens;
		}

		/// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
		public IList<InputParameter> InputParameters  { get; internal set; }
		/// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
		public IList<ReceivingStorageArea> OutputParameters { get; internal set; }
		/// <summary>RETURNING dataname</summary>
		public ReceivingStorageArea ReturningParameter { get; set; }
	}
}

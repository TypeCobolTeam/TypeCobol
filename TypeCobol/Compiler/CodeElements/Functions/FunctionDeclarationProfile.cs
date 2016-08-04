namespace TypeCobol.Compiler.CodeElements.Functions {

using System.Collections.Generic;

public class FunctionDeclarationProfile: CodeElement {

	/// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
	public IList<ParameterDescription> InputParameters  { get; internal set; }
	public SyntaxProperty<Passing.Mode> Input { get; internal set; }

	/// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
	public IList<ParameterDescription> OutputParameters { get; internal set; }
	public SyntaxProperty<Passing.Mode> Output { get; internal set; }

	/// <summary>INOUT datanames, always passed BY REFERENCE.</summary>
	public IList<ParameterDescription> InoutParameters { get; internal set; }
	public SyntaxProperty<Passing.Mode> Inout { get; internal set; }

	/// <summary>RETURNING dataname.</summary>
	public ParameterDescription ReturningParameter { get; set; }
	public SyntaxProperty<Passing.Mode> Returning { get; internal set; }

	public FunctionDeclarationProfile(): base(CodeElementType.ProcedureDivisionHeader) {
		InputParameters  = new List<ParameterDescription>();
		OutputParameters = new List<ParameterDescription>();
		InoutParameters  = new List<ParameterDescription>();
		ReturningParameter = null;
	}

	/// <summary>Only called if there are no input/using parameters.</summary>
	public FunctionDeclarationProfile(ProcedureDivisionHeader other): this() {
		if (other.ReturningParameter != null) {
			this.ReturningParameter = new ParameterDescription();
			this.ReturningParameter.DataName = other.ReturningParameter;
			this.ReturningParameter.Picture = null;//TODO#245 
		}
		this.ConsumedTokens = other.ConsumedTokens;
	}
}

}

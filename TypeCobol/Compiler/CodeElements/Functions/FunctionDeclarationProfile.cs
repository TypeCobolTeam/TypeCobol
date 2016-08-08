namespace TypeCobol.Compiler.CodeElements.Functions {

using System;
using System.Collections.Generic;

public class FunctionDeclarationProfile: CodeElement {

	/// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
	public SyntaxProperty<Passing.Mode> Input { get; internal set; }
	/// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
	public SyntaxProperty<Passing.Mode> Output { get; internal set; }
	/// <summary>INOUT datanames, always passed BY REFERENCE.</summary>
	public SyntaxProperty<Passing.Mode> Inout { get; internal set; }
	/// <summary>RETURNING dataname.</summary>
	public SyntaxProperty<Passing.Mode> Returning { get; internal set; }

	public ParametersProfile Profile { get; private set; }

	public FunctionDeclarationProfile(): base(CodeElementType.ProcedureDivisionHeader) {
		Profile = new ParametersProfile();
	}

	/// <summary>Only called if there are no input/using parameters.</summary>
	public FunctionDeclarationProfile(ProcedureDivisionHeader other): this() {
		if (other.ReturningParameter != null) {
			Profile.ReturningParameter = new ParameterDescription();
			Profile.ReturningParameter.DataName = other.ReturningParameter;
			Profile.ReturningParameter.Picture = null;//TODO#245
		}
		this.ConsumedTokens = other.ConsumedTokens;
	}
}

}

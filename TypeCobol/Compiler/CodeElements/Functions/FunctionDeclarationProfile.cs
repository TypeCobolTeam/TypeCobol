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

	/// <summary>Only called if there are no INPUT/OUTPUT/INOUT/USING parameters.</summary>
	public FunctionDeclarationProfile(ProcedureDivisionHeader other): this() {
		if (other.UsingParameters != null && other.UsingParameters.Count > 0)
			throw new System.InvalidOperationException("Implementation error #245");
		if (other.ReturningParameter != null) {
			// we might have a RETURNING parameter to convert, but only if there is neither
			// PICTURE nor TYPE clause for the returning parameter in the function declaration.
			// however, this is as syntax error.
			Profile.ReturningParameter = new ParameterDescription();
			Profile.ReturningParameter.DataName = other.ReturningParameter;
			Profile.ReturningParameter.Picture = null;// we can't do anything here
		}
		this.ConsumedTokens = other.ConsumedTokens;
	}
}

}

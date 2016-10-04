namespace TypeCobol.Compiler.CodeElements.Functions {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;



public class FunctionDeclarationHeader: CodeElement {

	public QualifiedName Name { get; private set; }
	public AccessModifier Visibility { get; private set; }

	public FunctionDeclarationHeader(QualifiedName Name, AccessModifier Visibility)
		: base(CodeElementType.FunctionDeclarationHeader) {
		this.Name = Name;
		this.Visibility = Visibility;
		this.Profile = new ParametersProfile();
	}
	public void SetLibrary(string libname) {
		Name = new URI(libname + "." + Name);
	}

	 // PROFILE
	/////////////
	public ParametersProfile Profile { get; private set; }
	/// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
	public SyntaxProperty<Passing.Mode> Input { get; internal set; }
	/// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
	public SyntaxProperty<Passing.Mode> Output { get; internal set; }
	/// <summary>INOUT datanames, always passed BY REFERENCE.</summary>
	public SyntaxProperty<Passing.Mode> Inout { get; internal set; }
	/// <summary>RETURNING dataname.</summary>
	public SyntaxProperty<Passing.Mode> Returning { get; internal set; }
}


public class FunctionDeclarationEnd: CodeElement {
	public FunctionDeclarationEnd(): base(CodeElementType.FunctionDeclarationEnd) { }
}

}

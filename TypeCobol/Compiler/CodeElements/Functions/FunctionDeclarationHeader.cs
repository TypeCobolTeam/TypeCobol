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
	}
	public void SetLibrary(string libname) {
		Name = new URI(libname + "." + Name);
	}
}


public class FunctionDeclarationEnd: CodeElement {
	public FunctionDeclarationEnd(): base(CodeElementType.FunctionDeclarationEnd) { }
}

}

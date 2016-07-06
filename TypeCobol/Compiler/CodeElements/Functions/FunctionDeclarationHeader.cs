using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Functions;

namespace TypeCobol.Compiler.CodeElements {

	public class FunctionDeclarationHeader: CodeElement {

		public QualifiedName Name { get; private set; }
		public AccessModifier Visibility { get; private set; }

		public FunctionDeclarationHeader(QualifiedName Name, AccessModifier Visibility)
			: base(CodeElementType.ObjectIdentification) {//TODO CodeElementType.FunctionDeclarationHeader
			this.Name = Name;
			this.Visibility = Visibility;
		}

	}
}

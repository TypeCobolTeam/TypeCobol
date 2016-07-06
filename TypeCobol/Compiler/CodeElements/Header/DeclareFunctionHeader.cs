using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements {

	public class DeclareFunctionHeader: CodeElement {

		public QualifiedName Name { get; private set; }
		public AccessModifier Visibility { get; private set; }

		public DeclareFunctionHeader(QualifiedName Name, AccessModifier Visibility)
			: base(CodeElementType.ParagraphHeader) {//TODO CodeElementType.DeclareFunctionHeader!
			this.Name = Name;
			this.Visibility = Visibility;
		}



		public enum AccessModifier {
			Public,
			Private
		}

	}
}

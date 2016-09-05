namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Functions;

public class ProcedureDivision: Node, CodeElementHolder<ProcedureDivisionHeader> {
	public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	public override string ID { get { return "procedure-division"; } }
}

// [TYPECOBOL]

public class FunctionDeclaration: Node, CodeElementHolder<FunctionDeclarationHeader> {
	public FunctionDeclaration(FunctionDeclarationHeader header): base(header) { }
	public override string ID { get { return "function-declaration"; } }

	public override string Name { get { return QualifiedName.Head; } }
	public override CodeElements.Expressions.QualifiedName QualifiedName { get { return this.CodeElement().Name; } }

	public string Library { get; internal set; }
	public string Copy { get { return Library+"cpy"; } }
	public ParametersProfile Profile {
		get {
			var list = GetChildren<FunctionDeclarationProfile>();
			if (list.Count < 1) return null;
			var profile = list[list.Count-1].CodeElement().Profile;
			return list[list.Count-1].CodeElement().Profile;
		}
	}
}

public class FunctionProfile: Node, CodeElementHolder<FunctionDeclarationProfile> {
	public FunctionProfile(FunctionDeclarationProfile profile): base(profile) { }
	public override string ID { get { return "function-profile"; } }
}

public class FunctionEnd: Node, CodeElementHolder<FunctionDeclarationEnd> {
	public FunctionEnd(FunctionDeclarationEnd end): base(end) { }
	public override string ID { get { return "function-end"; } }
}

// [/TYPECOBOL]

public class Section: Node, CodeElementHolder<SectionHeader> {
	public Section(SectionHeader header): base(header) { }
	public override string ID { get { return this.CodeElement().SectionName.Name; } }
}

public class Paragraph: Node, CodeElementHolder<ParagraphHeader> {
	public Paragraph(ParagraphHeader header): base(header) { }
	public override string ID { get { return this.CodeElement().ParagraphName.Name; } }
}

public class Sentence: Node, CodeElementHolder<CodeElement> {
	public Sentence(): base(null) { }
}

} // end of namespace TypeCobol.Compiler.Nodes

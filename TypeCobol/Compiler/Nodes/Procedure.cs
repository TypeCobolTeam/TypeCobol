namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public class ProcedureDivision: Node, CodeElementHolder<ProcedureDivisionHeader> {
	public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	public override string ID { get { return "procedure-division"; } }
}

// [TYPECOBOL]

public class FunctionDeclaration: Node, CodeElementHolder<FunctionDeclarationHeader> {
	public FunctionDeclaration(FunctionDeclarationHeader header): base(header) { }
	public override string ID { get { return Name; } }
	public string Label { get; internal set; }

	public override string Name { get { return QualifiedName.Head; } }
	public override CodeElements.Expressions.QualifiedName QualifiedName { get { return new CodeElements.Expressions.URI(this.CodeElement().Name); } }

	public string Library { get; internal set; }
	public string Copy { get { return Library+"cpy"; } }
	public ParametersProfile Profile { get { return this.CodeElement().Profile; } }
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

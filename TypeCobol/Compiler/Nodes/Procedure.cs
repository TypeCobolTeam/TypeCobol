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

namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Functions;

public class ProcedureDivision: Node<ProcedureDivisionHeader> {
	public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	public override string ID { get { return "procedure-division"; } }
}

// [TYPECOBOL]

public class FunctionDeclaration: Node<FunctionDeclarationHeader> {
	public FunctionDeclaration(FunctionDeclarationHeader header): base(header) { }
	public override string ID { get { return "function-declaration"; } }
}

public class FunctionProfile: Node<FunctionDeclarationProfile> {
	public FunctionProfile(FunctionDeclarationProfile profile): base(profile) { }
	public override string ID { get { return "function-profile"; } }
}

public class FunctionEnd: Node<FunctionDeclarationEnd> {
	public FunctionEnd(FunctionDeclarationEnd end): base(end) { }
	public override string ID { get { return "function-end"; } }
}

// [/TYPECOBOL]

public class Section: Node<SectionHeader> {
	public Section(SectionHeader header): base(header) { }
	public override string ID { get { return CodeElement.SectionName.Name; } }
}

public class Paragraph: Node<ParagraphHeader> {
	public Paragraph(ParagraphHeader header): base(header) { }
	public override string ID { get { return CodeElement.ParagraphName.Name; } }
}

public class Sentence: Node<CodeElement> {
	public Sentence(): base(null) { }
}

} // end of namespace TypeCobol.Compiler.Nodes

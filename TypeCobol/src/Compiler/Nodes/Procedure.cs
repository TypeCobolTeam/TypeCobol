namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Functions;

public class ProcedureDivision: CodeElementNode<ProcedureDivisionHeader> {
	public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	public override string ID { get { return "procedure-division"; } }
}

// [TYPECOBOL]

public class FunctionDeclaration: CodeElementNode<FunctionDeclarationHeader> {
	public FunctionDeclaration(FunctionDeclarationHeader header): base(header) { }
	public override string ID { get { return "function-declaration"; } }
}

public class FunctionProfile: CodeElementNode<FunctionDeclarationProfile> {
	public FunctionProfile(FunctionDeclarationProfile profile): base(profile) { }
	public override string ID { get { return "function-profile"; } }
}

public class FunctionEnd: CodeElementNode<FunctionDeclarationEnd> {
	public FunctionEnd(FunctionDeclarationEnd end): base(end) { }
	public override string ID { get { return "function-end"; } }
}

// [/TYPECOBOL]

public class Section: CodeElementNode<SectionHeader> {
	public Section(SectionHeader header): base(header) { }
	public override string ID { get { return CodeElement.SectionName.Name; } }
}

public class Paragraph: CodeElementNode<ParagraphHeader> {
	public Paragraph(ParagraphHeader header): base(header) { }
	public override string ID { get { return CodeElement.ParagraphName.Name; } }
}

public class Sentence: CodeElementNode<CodeElement> {
	public Sentence(): base(null) { }
}

} // end of namespace TypeCobol.Compiler.Nodes

namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public class ProcedureDivision: CodeElementNode<ProcedureDivisionHeader> {
	public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	public override string ID { get { return "procedure-division"; } }
}

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

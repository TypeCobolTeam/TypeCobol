namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

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

} // end of namespace TypeCobol.Compiler.Nodes

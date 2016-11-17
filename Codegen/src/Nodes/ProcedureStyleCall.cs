namespace TypeCobol.Codegen.Nodes {
	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Text;

internal class ProcedureStyleCall: Compiler.Nodes.Call, Generated {
	private Compiler.Nodes.ProcedureStyleCall Node;
	private CallStatement call;

	public ProcedureStyleCall(Compiler.Nodes.ProcedureStyleCall node)
		: base(null) {
		this.Node = node;
		var statement = (ProcedureStyleCallStatement)Node.CodeElement;
		call = new CallStatement();
		call.ProgramOrProgramEntryOrProcedureOrFunction = new SymbolReferenceVariable(StorageDataType.ProgramName, statement.ProcedureCall.ProcedureName);
		call.InputParameters = new List<CallSiteParameter>(statement.ProcedureCall.Arguments);
		call.OutputParameter = null;
	}

	public override CodeElement CodeElement { get { return this.Node.CodeElement; } }

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				var statement = (ProcedureStyleCallStatement)Node.CodeElement;

				var hash = GetHash(statement.ProcedureCall);
				var callTextLine = new TextLineSnapshot(-1, "    CALL " + hash + " USING ", null);
				_cache.Add(callTextLine);
				var prefix = new string(' ', callTextLine.Length + 1);
				foreach (var parameter in call.InputParameters) {
					_cache.Add(new TextLineSnapshot(-1, prefix + parameter.StorageAreaOrValue, null));
				}
			}
			return _cache;
		}
	}
	private string GetHash(ProcedureCall call) {
		var name = new Compiler.CodeElements.Expressions.URI(call.ProcedureName.Name);
		var found = Node.SymbolTable.GetFunction(name, call.AsProfile(Node.SymbolTable));
		if (found.Count < 1) return "?NOT_FOUND?";
		if (found.Count > 1) return "?AMBIGUOUS?";
		return ((Compiler.Nodes.FunctionDeclaration)found[0]).Hash;
	}

	public bool IsLeaf { get { return true; } }
}

}

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
				_cache.Add(new TextLineSnapshot(-1, "CALL "+statement.ProcedureCall.ProcedureName.Name, null));
				var str = new System.Text.StringBuilder("    USING ");
				foreach(var parameter in call.InputParameters) {
					str.Append(' ').Append(parameter.StorageAreaOrValue.ToString());
				}
				_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));
			}
			return _cache;
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

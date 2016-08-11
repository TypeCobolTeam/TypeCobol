namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;


/// <summary>
/// TODO#245: this should NOT be necessary.
/// Instead, grammar should be refactored so INPUT/OUTPUT/INOUT/RETURNING _AND_ USING parameters
/// are created as CodeElements and put in procedure header OR function profile node only in semantic phase.
/// </summary>
internal class ParameterEntry: Node<Compiler.CodeElements.Functions.ParameterDescription>, Generated {
	public Compiler.CodeElements.Functions.ParameterDescription Description { get; private set; }
	public ParameterEntry(Compiler.CodeElements.Functions.ParameterDescription description): base(description) { }

	private IList<ITextLine> _cache = null;
	IEnumerable<ITextLine> Generated.Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				var str = new System.Text.StringBuilder();
				str.Append("    ");// indent
				// TCRFUN_CODEGEN_PARAMETERS_IN_LINKAGE_SECTION
				str.Append("01 ").Append(CodeElement.DataName.Name).Append(" PIC ").Append(CodeElement.Picture);
				_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

}

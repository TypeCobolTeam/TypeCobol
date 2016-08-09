namespace TypeCobol.Codegen.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Text;


/// <summary>
/// TODO#245: this should NOT be necessary.
/// Instead, grammar should be refactored so INPUT/OUTPUT/INOUT/RETURNING _AND_ USING parameters
/// are created as CodeElements and put in procedure header OR function profile node only in semantic phase.
/// </summary>
internal class ParameterEntry: Node, Generated {
	public Compiler.CodeElements.Functions.ParameterDescription Description { get; private set; }
	public ParameterEntry(Compiler.CodeElements.Functions.ParameterDescription description) {
		this.Description = description;
	}

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				var str = new System.Text.StringBuilder();
				str.Append("    ");// indent
				// TCRFUN_CODEGEN_PARAMETERS_IN_LINKAGE_SECTION
				str.Append("01 ").Append(Description.DataName.Name).Append(" PIC ").Append(Description.Picture);
				_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

}

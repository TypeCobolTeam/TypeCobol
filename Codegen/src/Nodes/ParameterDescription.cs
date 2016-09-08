namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.CodeModel;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;


/// <summary>
/// TODO#245: this should NOT be necessary.
/// Instead, grammar should be refactored so INPUT/OUTPUT/INOUT/RETURNING _AND_ USING parameters
/// are created as CodeElements and put in procedure header OR function profile node only in semantic phase.
/// </summary>
internal class ParameterEntry: Node, CodeElementHolder<Compiler.CodeElements.Functions.ParameterDescriptionEntry>, Generated {
	public Compiler.CodeElements.Functions.ParameterDescription Description { get; private set; }
	public ParameterEntry(Compiler.CodeElements.Functions.ParameterDescriptionEntry entry, SymbolTable table): base(entry) {
		this.SymbolTable = table;
	}

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				var str = new System.Text.StringBuilder();
				// TCRFUN_CODEGEN_PARAMETERS_IN_LINKAGE_SECTION
//TODO#249				if (Description.IsConditionNameDescription) {
//					str.Append("88 ").Append(Description.Name.Name); //TODO value
//					if (Description.InitialValue != null) str.Append(" VALUE ").Append(Description.InitialValue.ToString());
//					if (Description.ThroughValue != null) str.Append(' ').Append(Description.ThroughValue.ToString());
//				} else {
					str.Append("01 ").Append(this.CodeElement().Name);
					if(this.CodeElement().Picture != null) str.Append(" PIC ").Append(this.CodeElement().Picture);
//				}
				str.Append('.');
				_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));

				if (!this.CodeElement().DataType.IsCOBOL) {
					var customtype = this.SymbolTable.GetType(new URI(this.CodeElement().DataType.Name));
					if (customtype.Count > 0) _cache.AddRange(TypedDataNode.InsertChildren(this.SymbolTable, (TypeDefinition)customtype[0], 2, 1));
				}
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

}

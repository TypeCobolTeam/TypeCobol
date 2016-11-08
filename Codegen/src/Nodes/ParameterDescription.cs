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
internal class ParameterEntry: Node, CodeElementHolder<ParameterDescriptionEntry>, Generated {
	public ParameterDescription Description { get; private set; }
	public ParameterEntry(ParameterDescriptionEntry entry, SymbolTable table): base(entry) {
		this.SymbolTable = table;
	}

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				string name = this.CodeElement().Name;
				_cache = new List<ITextLine>();
				if (this.CodeElement().DataType == DataType.Boolean) {
					_cache.Add(new TextLineSnapshot(-1, "01 "+name+"-value PIC X     VALUE LOW-VALUE.", null));
					_cache.Add(new TextLineSnapshot(-1, "    88 "+name+"       VALUE 'T'.", null));
					_cache.Add(new TextLineSnapshot(-1, "    88 "+name+"-false VALUE 'F'.", null));
				} else {
					var str = new System.Text.StringBuilder();
					str.Append("01 ").Append(name);
					if(this.CodeElement().Picture != null) str.Append(" PIC ").Append(this.CodeElement().Picture);
					str.Append('.');
					_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));

					// TCRFUN_CODEGEN_PARAMETERS_IN_LINKAGE_SECTION
					foreach(var child in GetChildren<DataConditionEntry>()) {
						str.Clear();
						var entry = child.CodeElement();
						str.Append("    ").Append("88 ").Append(entry.Name);
						if (entry.ConditionValues != null && entry.ConditionValues.Length > 0) {
							str.Append(" VALUE");
							foreach(var value in entry.ConditionValues)
								str.Append(" \'").Append(value.ToString()).Append('\'');
						} else
						if (entry.ConditionValuesRanges != null && entry.ConditionValuesRanges.Length > 0) {
							str.Append(" VALUES");
							foreach(var range in entry.ConditionValuesRanges)
								str.Append(" \'").Append(range.MinValue.ToString()).Append("\' THRU \'").Append(range.MaxValue.ToString()).Append('\'');
						}
						str.Append('.');
						_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));
					}
				}

				if (!this.CodeElement().DataType.IsCOBOL) {
					var customtype = this.SymbolTable.GetType(new URI(this.CodeElement().DataType.Name));
					if (customtype.Count > 0) _cache.AddRange(TypedDataNode.InsertChildren(this.SymbolTable, (TypeDefinition)customtype[0], 2, 1));
				}
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return true; } }
}

}

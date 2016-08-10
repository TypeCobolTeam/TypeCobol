namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Text;



internal class TypedDataNode: Compiler.Nodes.DataDescription, Generated {

	private Compiler.Nodes.DataDescription Node;

	public TypedDataNode(Compiler.Nodes.DataDescription node)
		: base(null) { this.Node = node; }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				int level = (int)Node.CodeElement.LevelNumber.Value;
				int generation = this.Node.Generation;
				var type = this.Node.SymbolTable.GetCustomType(Node.CodeElement.CustomType.Value);
				bool isCustomType = type != null;
				_cache.Add(CreateDataDefinition(Node.CodeElement, level, generation, true));
				InsertChildren(Node, level+1, generation+1);
			}
			return _cache;
		}
	}

	internal ITextLine CreateDataDefinition(DataDescriptionEntry data, int level, int generation, bool isCustomType) {
		var line = new System.Text.StringBuilder();
		for(int c = 0; c < generation; c++) line.Append("  ");
		line.Append(level.ToString("00")).Append(' ').Append(data.DataName.Name);
		if (!isCustomType) line.Append(" PIC ").Append(data.Picture);
		line.Append('.');
		return new TextLineSnapshot(-1, line.ToString(), null);
	}

	private void InsertChildren(Compiler.Nodes.DataDescription type, int level, int generation) {
		foreach(var child in type.GetChildren()) {
			bool isCustomTypeToo = /*!(child.CodeElement is TypeDefinitionEntry) &&*/ Node.SymbolTable.IsCustomType(child.CodeElement.CustomType.Value);
			_cache.Add(CreateDataDefinition(child, level, generation, isCustomTypeToo));
			if (isCustomTypeToo) InsertChildren(Node.SymbolTable.GetCustomType(child.CodeElement.CustomType.Value), level+1, generation+1);
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

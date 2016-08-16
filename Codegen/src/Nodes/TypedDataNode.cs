namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;



internal class TypedDataNode: DataDescription, Generated {

	private DataDescription node;

	public TypedDataNode(DataDescription node)
		: base(null) { this.node = node; }

	private IList<ITextLine> _cache = null;
	IEnumerable<ITextLine> Generated.Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				var data = (DataDescriptionEntry)node.CodeElement;
				int level = (int)data.LevelNumber.Value;
				int generation = this.node.Generation;
				var type = this.node.SymbolTable.GetCustomType(data.CustomType.Value);
				bool isCustomType = type != null;
				_cache.Add(CreateDataDefinition(data, level, generation, true));
				InsertChildren(this.node, level+1, generation+1);
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

	private void InsertChildren(DataDefinition typed, int level, int generation) {
		foreach(var child in typed.Children) {
			var data = (DataDescriptionEntry)child.CodeElement;
			TypeDefinition customtype = null;
			try { customtype = node.SymbolTable.GetCustomType(data.CustomType.Value); }
			catch(System.ArgumentException) { }//DO nothing
			bool isCustomTypeToo = !(data is TypeDefinition) && customtype != null;
			_cache.Add(CreateDataDefinition(data, level, generation, isCustomTypeToo));
			if (isCustomTypeToo) InsertChildren(customtype, level+1, generation+1);
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

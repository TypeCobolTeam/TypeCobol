namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;



internal class TypedDataNode: DataDescription, Generated {

	private DataDescription Node;

	public TypedDataNode(DataDescription node)
		: base(null) { this.Node = node; }

	private IList<ITextLine> _cache = null;
	IEnumerable<ITextLine> Generated.Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				int level = (int)Node.CodeElement.LevelNumber.Value;
				int generation = this.Node.Generation;
				var type = this.Node.SymbolTable.GetCustomType(Node.CodeElement.CustomType.Value);
				bool isCustomType = type != null;
				_cache.Add(CreateDataDefinition(Node.CodeElement, level, generation, true));
				InsertChildren((Node<CodeElement>)(object)Node, level+1, generation+1);
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

	private void InsertChildren(Node<CodeElement> typed, int level, int generation) {
		foreach(var c in typed.GetChildren()) {
			var child = (DataDescription)(object)c;
			TypeDescription customtype = null;
			try { customtype = Node.SymbolTable.GetCustomType(child.CodeElement.CustomType.Value); }
			catch(System.ArgumentException) { }//DO nothing
			bool isCustomTypeToo = !(child.CodeElement is TypeDescription) && customtype != null;
			_cache.Add(CreateDataDefinition(child.CodeElement, level, generation, isCustomTypeToo));
			if (isCustomTypeToo) InsertChildren((Node<CodeElement>)(object)customtype, level+1, generation+1);
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

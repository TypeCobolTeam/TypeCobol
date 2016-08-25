namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;



internal class TypedDataNode: DataDescription, Generated {

	private DataDescription node;
/*=======
		public TypedDataNode(Node node) {
			this.Node = node;
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>();
					var data = this.Node.CodeElement as DataDescriptionEntry;
					var type = this.Node.SymbolTable.GetCustomType(data.DataType.Name);
					int level = data.LevelNumber;
					int generation = data.Generation;
					bool isCustomType = type != null;
					_cache.Add(CreateDataDefinition(data, level, generation));
					InsertChildren(data, level+1, generation+1);
				}
				return _cache;
			}
		}

		internal ITextLine CreateDataDefinition(DataDescriptionEntry data, int level, int generation) {
			var line = new System.Text.StringBuilder();
			for(int c = 0; c < generation; c++) line.Append("  ");
			line.Append(level.ToString("00")).Append(' ').Append(data.Name.Name);

			string picture;
			try {
				var type = Node.SymbolTable.GetCustomType(data.DataType.Name);
				if (type.Subordinates.Count > 0) picture = null;
				else picture = ((DataDescriptionEntry)type).Picture;
			} catch(System.ArgumentException) { picture = data.Picture; }
			if (picture != null) line.Append(" PIC ").Append(picture);
			line.Append('.');
			return new TextLineSnapshot(-1, line.ToString(), null);
		}
>>>>>>> 2ba2545a0a0fbf93388c4348f16676eddcfa3069
*/
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
/*=======
		private void InsertChildren(TypeDefinition type, int level, int generation) {
			foreach(var child in type.Subordinates) {
				bool isCustomTypeToo = !child.IsTypeDefinition && Node.SymbolTable.IsCustomType(child.DataType);
				_cache.Add(CreateDataDefinition(child, level, generation));
				if (isCustomTypeToo) InsertChildren(Node.SymbolTable.GetCustomType(child.DataType.Name), level+1, generation+1);
>>>>>>> 2ba2545a0a0fbf93388c4348f16676eddcfa3069
*/			}
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

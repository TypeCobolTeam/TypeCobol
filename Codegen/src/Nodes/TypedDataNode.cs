namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;



internal class TypedDataNode: DataDescription, Generated {

	private DataDescription Node;
	public TypedDataNode(DataDescription node): base(null) { this.Node = node; }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				if (this.Node.IsPartOfATypeDef) return _cache;

				var data = this.Node.CodeElement();
				int level = (int)data.LevelNumber.Value;
				_cache.Add(CreateDataDefinition(data, level, 0, true));

				var customtype = this.Node.SymbolTable.GetType(new URI(data.DataType.Name));
				if (customtype.Count > 0) InsertChildren((TypeDefinition)customtype[0], level+1, 1);
			}
			return _cache;
		}
	}

	internal ITextLine CreateDataDefinition(DataDescriptionEntry data, int level, int indent, bool isCustomType) {
		var line = GetIndent(level, indent);
		line.Append(level.ToString("00")).Append(' ').Append(data.DataName.Name);
		if (isCustomType) {
			line.Append('.');
			foreach(var type in this.Node.SymbolTable.GetType(new URI(data.DataType.Name))) {
				System.Console.WriteLine("%%% ctype: "+type.Name+" #"+((Node)type).Children.Count);
			}
		} else line.Append(" PIC ").Append(data.Picture).Append('.');
		return new TextLineSnapshot(-1, line.ToString(), null);
	}

	private System.Text.StringBuilder GetIndent(int level, int indent) {
		var str = new System.Text.StringBuilder();
		if (level == 1 || level == 77) return str;
		str.Append("    ");
		for(int i=0; i<indent; i++) str.Append("  ");
		return str;
	}

	private void InsertChildren(DataDefinition type, int level, int indent) {
		foreach(var child in type.Children) {
			if (child is TypedDataNode) continue;
			var typed = (Typed)child;
			var types = this.Node.SymbolTable.GetType(new URI(typed.DataType.Name));
			bool isCustomTypeToo = !(child is TypeDefinition) && (types.Count > 0);
			_cache.Add(CreateDataDefinition((DataDescriptionEntry)child.CodeElement, level, indent, isCustomTypeToo));
			if (isCustomTypeToo) InsertChildren((TypeDefinition)types[0], level+1, indent+1);
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

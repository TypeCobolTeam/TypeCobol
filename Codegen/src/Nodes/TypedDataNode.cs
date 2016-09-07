namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.CodeModel;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;



internal class TypedDataNode: DataDescription, Generated {

	private DataDescription Node;
	public TypedDataNode(DataDescription node): base(null) { this.Node = node; }

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				if (this.Node.IsPartOfATypeDef) return _cache;

				var data = this.Node.CodeElement();
				int level = (int)data.LevelNumber.Value;
				_cache.Add(CreateDataDefinition(data, level, 0, true));

				var customtype = this.Node.SymbolTable.GetType(new URI(data.DataType.Name));
				if (customtype.Count > 0) _cache.AddRange(InsertChildren(this.Node.SymbolTable, (TypeDefinition)customtype[0], level+1, 1));
			}
			return _cache;
		}
	}

	internal static ITextLine CreateDataDefinition(DataDescriptionEntry data, int level, int indent, bool isCustomType) {
		var line = GetIndent(level, indent);
		line.Append(level.ToString("00"));
		if (data.Name != null) line.Append(' ').Append(data.Name);
		if (!isCustomType) line.Append(" PIC ").Append(data.Picture);
		line.Append('.');
		return new TextLineSnapshot(-1, line.ToString(), null);
	}

	private static System.Text.StringBuilder GetIndent(int level, int indent) {
		var str = new System.Text.StringBuilder();
		if (level == 1 || level == 77) return str;
		str.Append("    ");
		for(int i=1; i<indent; i++) str.Append("  ");
		return str;
	}

	public static List<ITextLine> InsertChildren(SymbolTable table, DataDefinition type, int level, int indent) {
		var lines = new List<ITextLine>();
		foreach(var child in type.Children) {
			if (child is TypedDataNode) continue;
			var typed = (Typed)child;
			var types = table.GetType(new URI(typed.DataType.Name));
			bool isCustomTypeToo = !(child is TypeDefinition) && (types.Count > 0);
			lines.Add(CreateDataDefinition((DataDescriptionEntry)child.CodeElement, level, indent, isCustomTypeToo));
			if (isCustomTypeToo) lines.AddRange(InsertChildren(table, (TypeDefinition)types[0], level+1, indent+1));
		}
		return lines;
	}

	public bool IsLeaf { get { return true; } }
}

}

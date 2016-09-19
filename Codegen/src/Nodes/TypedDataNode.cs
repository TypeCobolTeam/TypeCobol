using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class TypedDataNode: Node, Generated {

		private Node Node;

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


		private void InsertChildren(TypeDefinition type, int level, int generation) {
			foreach(var child in type.Subordinates) {
				bool isCustomTypeToo = !child.IsTypeDefinition && Node.SymbolTable.IsCustomType(child.DataType);
				_cache.Add(CreateDataDefinition(child, level, generation));
				if (isCustomTypeToo) InsertChildren(Node.SymbolTable.GetCustomType(child.DataType.Name), level+1, generation+1);
			}
		}

		public bool IsLeaf { get { return true; } }
	}
}

using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons.Templates;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class GeneratedNode: Node, Generated {
		private Solver Solver;

		public GeneratedNode(TypeCobol.Codegen.Skeletons.Templates.RazorEngine solver): base(null) {
			this.Solver = solver;
		}

		private IList<ITextLine> _cache = null;
		IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					string text = ((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)Solver).Replace();
					_cache = new List<ITextLine>();
					foreach(string line in text.Split()) {
						_cache.Add(new TextLineSnapshot(-1, line, null));
					}
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return true; } }
	}
}

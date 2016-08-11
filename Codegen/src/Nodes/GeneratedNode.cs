using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons.Templates;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class GeneratedNode: Compiler.Nodes.Node<CodeElement>, Generated {
		private Solver Solver;

		public GeneratedNode(Solver solver): base(null) {
			this.Solver = solver;
		}

		private IList<ITextLine> _cache = null;
		IEnumerable<ITextLine> Generated.Lines {
			get {
				if (_cache == null) {
					string text = Solver.Replace();
					_cache = new List<ITextLine>();
					foreach(string line in text.Split('\n')) {
						_cache.Add(new TextLineSnapshot(-1, line, null));
					}
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return true; } }
	}
}

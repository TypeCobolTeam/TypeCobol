using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class ProgramEnd: Node, Generated {

		private QualifiedName ProgramName;
		public ProgramEnd(QualifiedName ProgramName) {
			this.ProgramName = ProgramName;
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>();
					_cache.Add(new TextLineSnapshot(-1, "END PROGRAM "+ProgramName.Head+".", null));
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return true; } }
	}
}

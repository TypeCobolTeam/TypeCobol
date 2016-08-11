namespace TypeCobol.Codegen.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Text;



internal class ProgramEnd: Compiler.Nodes.End, Generated {

	private QualifiedName ProgramName;
	public ProgramEnd(QualifiedName ProgramName): base(new Compiler.CodeElements.ProgramEnd()) {
		this.ProgramName = ProgramName;
	}

	private IList<ITextLine> _cache = null;
	IEnumerable<ITextLine> Generated.Lines {
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

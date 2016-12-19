namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Codegen.Skeletons.Templates;
	using TypeCobol.Compiler.Text;
	using TypeCobol.Compiler.CodeElements;

public interface Generated {
	/// <summary>Generated code.</summary>
	IEnumerable<ITextLine> Lines { get; }
	/// <summary>Must be treated as a leaf regarding codegen.</summary>
	bool IsLeaf { get; }
}

internal class GeneratedNode: Compiler.Nodes.Node, Generated {
	private Solver Solver;
	public GeneratedNode(Solver solver): base(null) { this.Solver = solver; }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
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

	public bool IsLeaf { get { return false; } }
}

}

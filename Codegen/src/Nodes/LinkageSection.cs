namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Text;



internal class LinkageSection: Compiler.Nodes.LinkageSection, Generated {
	public LinkageSection(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "LINKAGE SECTION.", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

}

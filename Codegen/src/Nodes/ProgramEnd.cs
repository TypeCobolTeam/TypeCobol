namespace TypeCobol.Codegen.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Text;



internal class ProgramEnd: Compiler.Nodes.End, Generated {

	private QualifiedName ProgramHashName;
    private string OriginalProcName; //Limited to 22 chars

    public ProgramEnd(QualifiedName programHashName, string originalProcName) : base(new Compiler.CodeElements.ProgramEnd()) {
		this.ProgramHashName = programHashName;
        this.OriginalProcName = originalProcName;


    }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
			    _cache.Add(new TextLineSnapshot(-1, "END PROGRAM " + ProgramHashName.Head + OriginalProcName + ".", null));
			}
			return _cache;
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

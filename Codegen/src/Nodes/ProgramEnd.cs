namespace TypeCobol.Codegen.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Text;



internal class ProgramEnd: Compiler.Nodes.End, Generated {

	private QualifiedName ProgramHashName;
    private string ProgramName; //Limited to 22 chars

    public ProgramEnd(QualifiedName programHashName, string programName) : base(new Compiler.CodeElements.ProgramEnd()) {
		this.ProgramHashName = programHashName;
        this.ProgramName = programName;


    }
    public ProgramEnd(string programName) : base(new Compiler.CodeElements.ProgramEnd())
    {
        this.ProgramName = programName;
        this.ProgramHashName = new URI("");
    }

        private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
                if(ProgramHashName.Head == "")
                {
                    _cache.Add(new TextLineSnapshot(-1, "", null));
                }
                _cache.Add(new TextLineSnapshot(-1, "END PROGRAM " + ProgramHashName.Head + ProgramName + ".", null));
            }
			return _cache;
		}
	}

	public bool IsLeaf { get { return true; } }
}

}

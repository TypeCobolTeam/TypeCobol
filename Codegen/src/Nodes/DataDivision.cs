namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Text;



internal class DataDivision: Compiler.Nodes.DataDivision, Generated {
	public DataDivision(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "DATA DIVISION.", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
    public TypeCobol.Compiler.Nodes.Node CommentedNode
    {
        get
        {
            return null;
        }
    }
}



internal class WorkingStorageSection: Compiler.Nodes.WorkingStorageSection, Generated {
	public WorkingStorageSection(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "WORKING-STORAGE SECTION.", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
    public TypeCobol.Compiler.Nodes.Node CommentedNode
    {
        get
        {
            return null;
        }
    }
}

internal class LocalStorageSection: Compiler.Nodes.LocalStorageSection, Generated {
	public LocalStorageSection(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "LOCAL-STORAGE SECTION.", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
    public TypeCobol.Compiler.Nodes.Node CommentedNode
    {
        get
        {
            return null;
        }
    }
}

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
    public TypeCobol.Compiler.Nodes.Node CommentedNode
    {
        get
        {
            return null;
        }
    }
}

internal class FileSection: Compiler.Nodes.FileSection, Generated {
	public FileSection(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "FILE SECTION.", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
    public TypeCobol.Compiler.Nodes.Node CommentedNode
    {
        get
        {
            return null;
        }
    }
}

}

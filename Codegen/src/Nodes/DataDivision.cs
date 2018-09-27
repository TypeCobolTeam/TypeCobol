using TypeCobol.Compiler.Nodes;

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
                if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                    _cache.Add(new TextLineSnapshot(-1, "", null));
				_cache.Add(new TextLineSnapshot(-1, "DATA DIVISION.", null));
            }
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}



internal class WorkingStorageSection: Compiler.Nodes.WorkingStorageSection, Generated {
	public WorkingStorageSection(): base(null) { }

	public WorkingStorageSection(FunctionDeclaration declare): base(null)
    {
        var signature = new List<TextLineSnapshot>();
        signature.Add(new TextLineSnapshot(-1,
            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
        signature.AddRange(declare.Profile.GetSignatureForComment());

        Signature = signature;
    }

    private readonly IEnumerable<TextLineSnapshot> Signature;
    private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
                if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                    _cache.Add(new TextLineSnapshot(-1, "", null));
				_cache.Add(new TextLineSnapshot(-1, "WORKING-STORAGE SECTION.", null));

			    if (Signature != null)
			    {
			        foreach (var signature in Signature)
			        {
			            _cache.Add(signature);
			        }
			    }
            }
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

internal class LocalStorageSection: Compiler.Nodes.LocalStorageSection, Generated {
	public LocalStorageSection(): base(null) { }

	public LocalStorageSection(FunctionDeclaration declare): base(null)
    {
        var signature = new List<TextLineSnapshot>();
        signature.Add(new TextLineSnapshot(-1,
            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
        signature.AddRange(declare.Profile.GetSignatureForComment());

        Signature = signature;
    }

    private readonly IEnumerable<TextLineSnapshot> Signature;
    private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
                if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                    _cache.Add(new TextLineSnapshot(-1, "", null));
				_cache.Add(new TextLineSnapshot(-1, "LOCAL-STORAGE SECTION.", null));


			    if (Signature != null)
			    {
			        foreach (var signature in Signature)
			        {
			            _cache.Add(signature);
			        }
			    }
            }
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

internal class LinkageSection: Compiler.Nodes.LinkageSection, Generated {
	public LinkageSection(): base(null) { }

    public LinkageSection(FunctionDeclaration declare) : base(null)
    {
        var signature = new List<TextLineSnapshot>();
        signature.Add(new TextLineSnapshot(-1,
            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
        signature.AddRange(declare.Profile.GetSignatureForComment());

        Signature = signature;
    }

    private readonly IEnumerable<TextLineSnapshot> Signature;
	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
                if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                    _cache.Add(new TextLineSnapshot(-1, "", null));
				_cache.Add(new TextLineSnapshot(-1, "LINKAGE SECTION.", null));

			    if (Signature != null)
			    {
			        foreach (var signature in Signature)
			        {
			            _cache.Add(signature);
			        }
			    }

			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

internal class FileSection: Compiler.Nodes.FileSection, Generated {
	public FileSection(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
                if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                    _cache.Add(new TextLineSnapshot(-1, "", null));
				_cache.Add(new TextLineSnapshot(-1, "FILE SECTION.", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return false; } }
}

}

namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using System.Text;
	using TypeCobol.Compiler.CodeElements;

    public class ProcedureDivision: Node, CodeElementHolder<ProcedureDivisionHeader> {
	    public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	    public override string ID { get { return "procedure-division"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    // [TYPECOBOL]

    public class FunctionDeclaration: Node, CodeElementHolder<FunctionDeclarationHeader>, Tools.Hashable {
	    public FunctionDeclaration(FunctionDeclarationHeader header): base(header) { }
	    public override string ID { get { return Name; } }
	    public string Label { get; internal set; }

	    public override string Name { get { return QualifiedName.Head; } }
	    public override CodeElements.Expressions.QualifiedName QualifiedName { get { return new CodeElements.Expressions.URI(this.CodeElement().Name); } }

	    public string Library { get; internal set; }
	    public string Copy { get { return Library+"cpy"; } }
	    public ParametersProfile Profile { get { return this.CodeElement().Profile; } }

	    public string Hash {
		    get {
			    var hash = new StringBuilder();
			    hash.Append(Library).Append('.').Append(Name);
			    encode(hash, Profile.InputParameters).Append(':');
			    encode(hash, Profile.InoutParameters).Append(':');
			    encode(hash, Profile.OutputParameters).Append(':');
			    hash.Append(encode(Profile.ReturningParameter));
			    return Tools.Hash.CreateCOBOLNameHash(hash.ToString(), 8, this);
		    }
	    }
	    private StringBuilder encode(StringBuilder str, IList<ParameterDescriptionEntry> parameters) {
		    str.Append('[');
		    foreach(var p in parameters) str.Append(encode(p)).Append(',');
		    if (parameters.Count > 0) str.Length -= 1;
		    str.Append(']');
		    return str;
	    }
	    private string encode(ParameterDescriptionEntry parameter) {
		    if (parameter == null) return "?";
		    if (parameter.Picture != null) return parameter.Picture.ToString();
		    if (parameter.DataType != null) return "T("+parameter.DataType.Name+")";
		    return "??";
	    }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class FunctionEnd: Node, CodeElementHolder<FunctionDeclarationEnd> {
	    public FunctionEnd(FunctionDeclarationEnd end): base(end) { }
	    public override string ID { get { return "function-end"; } }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

// [/TYPECOBOL]

    public class Section: Node, CodeElementHolder<SectionHeader> {
	    public Section(SectionHeader header): base(header) { }
	    public override string ID { get { return this.CodeElement().SectionName.Name; } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }
    }

    public class Paragraph: Node, CodeElementHolder<ParagraphHeader> {
	    public Paragraph(ParagraphHeader header): base(header) { }
	    public override string ID { get { return this.CodeElement().ParagraphName.Name; } }

        public override string GenURI
        {
            get
            {
                string id = string.Intern("paragraph");
                var puri = Parent == null ? null : Parent.GenURI;
                if (puri == null) return id;
                return puri + '.' + id;
            }
        }  

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }
    }

    public class Sentence: Node, CodeElementHolder<CodeElement> {
	    public Sentence(): base(null) { }
	    public override string ID {
		    get {
			    string id = "sentence-";
			    if (Parent == null) id += '0';
			    else id += new System.Collections.Generic.List<Node>(Parent.Children).IndexOf(this);
			    return id;
		    }
	    }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
} // end of namespace TypeCobol.Compiler.Nodes

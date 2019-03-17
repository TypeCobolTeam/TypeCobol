using System;
using System.IO;
using System.Xml.Serialization;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Nodes {

    using System.Collections.Generic;
    using System.Text;
    using TypeCobol.Compiler.CodeElements;
    using CodeElements.Expressions;

    public class ProcedureDivision: GenericNode<ProcedureDivisionHeader> {
	    public ProcedureDivision(ProcedureDivisionHeader header): base(header) { }
	    public override string ID { get { return "procedure-division"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        /// <summary>
        /// Specialization for issue: 
        /// Codegen for procedure : remove usage of external  #519 
        /// </summary>
        public override IEnumerable<TypeCobol.Compiler.Text.ITextLine> Lines
        {
            get
            {
                if (!this.IsFlagSet(Node.Flag.ProcedureDivisionUsingPntTabPnt))
                {
                    return base.Lines;
                }
                var lines = new List<TypeCobol.Compiler.Text.ITextLine>();
                if (CodeElement == null || CodeElement.ConsumedTokens == null) return lines;
                bool bPeriodSeen = false;
                string use = " USING PntTab-Pnt.";
                string sep = "";
                StringBuilder sb = new StringBuilder();
                bool insideFormalizedComment = false;
                int lastComputedLine = 0;
                foreach (var token in CodeElement.ConsumedTokens)
                {//JCM: Don't take in account imported token.
                    if (token.TokenType == TokenType.FORMALIZED_COMMENTS_START)
                        insideFormalizedComment = true;
                    if (insideFormalizedComment && lastComputedLine != token.TokensLine.LineIndex)
                    {
                        lastComputedLine = token.TokensLine.LineIndex;
                        string text = '*' + token.TokensLine.Text.Substring(7, token.TokensLine.Text.Length - 7);
                        lines.Add(new TypeCobol.Compiler.Text.TextLineSnapshot(-1, text, null));
                    }
                    else if (!(token is TypeCobol.Compiler.Preprocessor.ImportedToken) && !insideFormalizedComment)
                    {
                        if (token.TokenType == TypeCobol.Compiler.Scanner.TokenType.PeriodSeparator)
                        {
                            bPeriodSeen = true;
                            sb.Append(use);
                        }
                        else
                        {
                            sb.Append(sep);
                            sb.Append(token.Text);
                        }
                        sep = " ";
                    }
                    if (token.TokenType == TokenType.FORMALIZED_COMMENTS_STOP)
                        insideFormalizedComment = false;
                }
                if (!bPeriodSeen)
                    sb.Append(use);
                lines.Add(new TypeCobol.Compiler.Text.TextLineSnapshot(-1, sb.ToString(), null));

                if (IsFlagSet(Flag.InsideProcedure))
                {
                    var declare = Parent?.Parent as FunctionDeclaration;
                    if (declare != null)
                    {
                        lines.Add(new Text.TextLineSnapshot(-1,
                            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
                        lines.AddRange(declare.Profile.GetSignatureForComment());
                    }
                }

                return lines;
            }
        }
    }

    public class Declaratives : GenericNode<DeclarativesHeader>
    {
        public Declaratives(DeclarativesHeader header) : base(header) { }

        public override string ID { get { return "declaratives-header"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    // [TYPECOBOL]

    public class FunctionDeclaration: GenericNode<FunctionDeclarationHeader>, Tools.Hashable, IProcCaller, IDocumentable
    {
        public FunctionDeclaration(FunctionDeclarationHeader header) : base(header)
        {
            Profile = new ParametersProfileNode(null);
        }
	    public override string ID { get { return Name; } }
	    public string Label { get; internal set; }

	    public override string Name => this.CodeElement.FunctionName.Name;
        public override CodeElements.Expressions.QualifiedName QualifiedName { get { return new CodeElements.Expressions.URI(this.CodeElement.Name); } }

	    public string Library { get; internal set; }
	    public string Copy { get { return Library+"cpy"; } }
	    //public ParametersProfile Profile { get { return this.CodeElement().Profile; } }
        public ParametersProfileNode Profile{ get; set; }


        private string _hash;
	    public string Hash {
		    get
		    {
		        if (_hash != null) return _hash;
			    var hash = new StringBuilder();
			    hash.Append(Library).Append('.').Append(Name);
			    encode(hash, Profile.InputParameters).Append(':');
			    encode(hash, Profile.InoutParameters).Append(':');
			    encode(hash, Profile.OutputParameters).Append(':');
			    hash.Append(encode(Profile.ReturningParameter));
		        _hash = Tools.Hash.CreateCOBOLNameHash(hash.ToString(), 8, this);

		        return _hash;
		    }
	    }
	    private StringBuilder encode(StringBuilder str, IList<ParameterDescription> parameters) {
		    str.Append('[');
		    foreach(var p in parameters) str.Append(encode(p)).Append(',');
		    if (parameters.Count > 0) str.Length -= 1;
		    str.Append(']');
		    return str;
	    }
	    private string encode(ParameterDescription parameter) {
		    if (parameter == null) return "?";
		    if (parameter.Picture != null) return parameter.Picture.ToString();
		    if (parameter.DataType != null) return "T("+parameter.DataType.Name+")";
		    return "??";
	    }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }

        public Dictionary<string, Tuple<IList<SymbolReference>, ProcedureStyleCall>> ProcStyleCalls { get; set; }
    }

    public class FunctionEnd: GenericNode<FunctionDeclarationEnd> {
	    public FunctionEnd(FunctionDeclarationEnd end): base(end) { }
	    public override string ID { get { return "function-end"; } }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

// [/TYPECOBOL]

    public class Section: GenericNode<SectionHeader> {
	    public Section(SectionHeader header): base(header) { }
	    public override string ID { get { return "section"; } }
        public override string Name { get { return this.CodeElement.SectionName.Name; } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }
    }

    public class Paragraph: GenericNode<ParagraphHeader> {
	    public Paragraph(ParagraphHeader header): base(header) { }
	    public override string ID { get { return "paragraph"; } }
        private string _Name;
        public override string Name { get {
            if (_Name == null)
                _Name = this.CodeElement.ParagraphName.Name;
            return _Name;
        } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }
    }

    public class Sentence: Node {
	    public Sentence() { }
	    public override string ID {
		    get {
			    string id = "sentence-";
			    if (Parent == null) id += '0';
			    else id += new System.Collections.Generic.List<Node>(Parent.Children).IndexOf(this);
			    return id;
		    }
	    }

        protected override CodeElement InternalCodeElement => null;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
} // end of namespace TypeCobol.Compiler.Nodes

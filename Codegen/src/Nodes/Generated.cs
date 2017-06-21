namespace TypeCobol.Codegen.Nodes
{

    using System.Collections.Generic;
    using TypeCobol.Codegen.Skeletons.Templates;
    using TypeCobol.Compiler.Text;
    using TypeCobol.Compiler.CodeElements;
    using System;

    public interface Generated
    {
        /// <summary>Generated code.</summary>
        IEnumerable<ITextLine> Lines { get; }
        /// <summary>Must be treated as a leaf regarding codegen.</summary>
        bool IsLeaf { get; }
    }

    /// <summary>
    /// A Generated interface, which say the the generated code must be replaced as
    /// a whole text rather than as list of lines.
    /// </summary>
    public interface GeneratedAndReplace : Generated
    {
        /// <summary>Replace code.</summary>
        string ReplaceCode { get; }
    }

    internal class GeneratedNode : Compiler.Nodes.Node, Generated
    {
        private Solver Solver;
        /// <summary>
        /// Code Element to appy to this Generated Node
        /// </summary>
        private CodeElement ApplyCodeElement;
        public GeneratedNode(Solver solver) : base(null) { this.Solver = solver; }
        public GeneratedNode(Solver solver, CodeElement codelement) : base(null)
        {
            this.Solver = solver;
            ApplyCodeElement = codelement;
        }

        private IList<ITextLine> _cache = null;
        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                if (_cache == null)
                {
                    string text = Solver.Replace().TrimStart();
                    _cache = new List<ITextLine>();
                    if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                        _cache.Add(new TextLineSnapshot(-1, "", null));
                    foreach (string line in text.Split('\n'))
                    {
                        _cache.Add(new TextLineSnapshot(-1, line, null));
                    }
                }
                return _cache;
            }
        }

        /// <summary>
        /// Get Associated Code Element
        /// </summary>
        public override CodeElement CodeElement
        {
            get
            {
                return ApplyCodeElement != null ? ApplyCodeElement : base.CodeElement;
            }
        }

        public bool IsLeaf { get { return false; } }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            //Generated Node doesn't need to be visited
            return false;
        }
    }


    internal class GeneratedNode2 : Compiler.Nodes.Node, Generated
    {
        public GeneratedNode2(string text, bool isLeaf) : base(null) {
            this.Text = text;
            this.IsLeaf = isLeaf;
        }

        public bool IsLeaf { get; internal set; }

        public string Text { get; internal set; }

        private IList<ITextLine> _cache = null;
        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                if (_cache == null)
                {
                    _cache = new List<ITextLine>();
                    if (this.IsFlagSet(Flag.FactoryGeneratedNodeWithFirstNewLine))
                        _cache.Add(new TextLineSnapshot(-1, "", null));
                    foreach (string line in Text.Split('\n'))
                    {
                        _cache.Add(new TextLineSnapshot(-1, line, null));
                    }
                }
                return _cache;
            }
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            //Generated Node doesn't need to be visited
            return false;
        }
    }

    internal abstract class FakeGeneratedNode : Compiler.Nodes.Node, Generated
    {
        public FakeGeneratedNode(CodeElement CodeElement) : base(CodeElement) { }

        public bool IsLeaf
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return true;
        }
    }
    internal class GeneratedCodeElement : TypeCobol.Compiler.CodeElements.CodeElement
    {
        public GeneratedCodeElement(TypeCobol.Compiler.Scanner.Token token) : base((CodeElementType)0)
        {
            base.ConsumedTokens = new List<TypeCobol.Compiler.Scanner.Token>();
            base.ConsumedTokens.Add(token);
        }
        public GeneratedCodeElement(List<TypeCobol.Compiler.Scanner.Token> consumedTokens)
                : base((CodeElementType)0)
        {
            base.ConsumedTokens = consumedTokens;
        }

    }

}

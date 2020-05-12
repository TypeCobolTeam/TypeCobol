namespace TypeCobol.Codegen.Nodes
{
    using System.Collections.Generic;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.Text;

    internal class ProgramEnd : Compiler.Nodes.End, Generated
    {
        private QualifiedName ProgramHashName;

        public int OriginalLine { get; }

        public ProgramEnd(QualifiedName programHashName, int originalLine)
            : base(new Compiler.CodeElements.ProgramEnd())
        {
            this.ProgramHashName = programHashName;
            this.OriginalLine = originalLine;
        }

        private IList<ITextLine> _cache = null;

        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                if (_cache == null)
                {
                    _cache = new List<ITextLine>();
                    _cache.Add(new TextLineSnapshot(-1, "END PROGRAM " + ProgramHashName.Head + ".", null));
                    _cache.Add(new TextLineSnapshot(-1, "", null));
                    _cache.Add(new TextLineSnapshot(-1, "", null));
                }

                return _cache;
            }
        }

        public bool IsLeaf => true;
    }
}

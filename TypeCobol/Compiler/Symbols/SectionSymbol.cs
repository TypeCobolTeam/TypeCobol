using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents a Cobol Section
    /// </summary>
    public class SectionSymbol : Symbol
    {
        public SectionSymbol(string name)
            : base(name, Kinds.Section)
        {
            Paragraphs = new Scope<ParagraphSymbol>(this);
        }

        /// <summary>
        /// Paragraph scope of the Section.
        /// </summary>
        public Scope<ParagraphSymbol> Paragraphs
        {
            get;
            protected set;
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitSectionSymbol(this, arg); }
    }
}

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
            Paragraphs = new Domain<ParagraphSymbol>(this);
        }

        /// <summary>
        /// Paragraph scope of the Section.
        /// </summary>
        public Domain<ParagraphSymbol> Paragraphs
        {
            get;
            protected set;
        }

        /// <summary>
        /// Enters a paragraph symbol in this Section.
        /// </summary>
        /// <param name="p">The paragraph to enter.</param>
        public virtual void AddParagraph(ParagraphSymbol p)
        {
            p.Owner = this;
            Paragraphs.Enter(p);
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitSectionSymbol(this, arg);
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents a Cobol Section
    /// </summary>
    public class SectionSymbol : Symbol
    {
        public SectionSymbol(String name)
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

        /// <summary>
        /// Enters a paragraph symbol in this Sections
        /// </summary>
        /// <param name="p">The paragraph to enter</param>
        public virtual void AddParagraph(ParagraphSymbol p)
        {
            p.Owner = this;
            Paragraphs.Enter(p);
        }

        /// <summary>
        /// Enters a paragraph symbol if it does not exists
        /// </summary>
        /// <param name="p">The Paragraph to eneter</param>
        /// <returns>true if the paragraph has been entered, false if a paragraph with the same name already exists.</returns>
        public bool AddParagraphIfNotExist(ParagraphSymbol p)
        {
            var tmp = Paragraphs.Lookup(p.Name);
            if (tmp == null)
            {
                AddParagraph(p);
                return true;
            }
            else
            {
                return false;
            }
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitSectionSymbol(this, arg); }
    }
}

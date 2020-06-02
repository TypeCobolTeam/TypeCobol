namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents a Cobol Paragraph
    /// </summary>
    public class ParagraphSymbol : Symbol
    {
        /// <summary>
        /// Named Constructor
        /// </summary>
        /// <param name="name"></param>
        public ParagraphSymbol(string name)
            : base(name, Kinds.Paragraph)
        {
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitParagraphSymbol(this, arg);
        }
    }
}

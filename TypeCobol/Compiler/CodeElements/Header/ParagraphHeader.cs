using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Paragraph
    /// A paragraph-name followed by a separator period, optionally followed by
    /// one or more sentences.
    /// </summary>
    public class ParagraphHeader : CodeElement
    {
        public ParagraphHeader() : base(CodeElementType.ParagraphHeader)
        { }

        /// <summary>
        /// A user-defined word that identifies a paragraph. 
        /// A paragraph-name, because it can be qualified, need not be unique.
        /// </summary>
        public ParagraphName ParagraphName { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return base.ToString() + "{ParagraphName=" + ParagraphName.Name + "}";
        }
    }
}

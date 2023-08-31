using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Paragraph
    /// A paragraph-name followed by a separator period, optionally followed by
    /// one or more sentences.
    /// </summary>
    public class ParagraphHeader : CodeElement, INamedCodeElement
    {
        public ParagraphHeader() : base(CodeElementType.ParagraphHeader)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;

        /// <summary>
        /// A user-defined word that identifies a paragraph. 
        /// A paragraph-name, because it can be qualified, need not be unique.
        /// </summary>
        public SymbolDefinition ParagraphName { get; set; }

        public string Name => ParagraphName?.Name;

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- ParagraphName = " + ParagraphName);
            return sb.ToString();
        }
    }
}

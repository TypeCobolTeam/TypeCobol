using System;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Section
    /// A section-header optionally followed by one or more paragraphs.
    /// </summary>
    public class SectionHeader : CodeElement
    {
        public SectionHeader() : base(CodeElementType.SectionHeader)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
        /// <summary>
        /// A user-defined word that identifies a section. A referenced
        /// section-name, because it cannot be qualified, must be unique
        /// within the program in which it is defined.
        /// </summary>
        public SymbolDefinition SectionName { get; set; }

        /// <summary>
        /// An integer or a positive signed numeric literal ranging in value
        /// from 0 through 99. Priority-number identifies a fixed segment or an
        /// independent segment that is to contain the section.
        /// A segment consists of all sections in a program that have the same
        /// priority-number. Priority-number determines whether a section is stored in
        /// a fixed segment or an independent segment at run time.
        /// Segments with a priority-number of 0 through 49 are fixed segments.
        /// Segments with a priority-number of 50 through 99 are independent
        /// segments.
        /// </summary>
        public IntegerValue PriorityNumber { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- SectionName = " + SectionName);
            if(PriorityNumber != null)
            {
                sb.AppendLine("- PriorityNumber = " + PriorityNumber);
            }
            return sb.ToString();
        }
    }
}

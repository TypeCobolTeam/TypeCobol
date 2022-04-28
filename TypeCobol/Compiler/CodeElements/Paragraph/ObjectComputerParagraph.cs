using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The OBJECT-COMPUTER paragraph specifies the system for which the object
    /// program is designated.
    /// </summary>
    public class ObjectComputerParagraph : CodeElement
    {
        public ObjectComputerParagraph() : base(CodeElementType.ObjectComputerParagraph)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
        /// <summary>
        /// computer-name
        /// A system-name. For example: IBM-system
        /// </summary>
        public AlphanumericValue ComputerName { get; set; }

        /// <summary>
        /// MEMORY SIZE integer
        /// integer specifies the amount of main storage needed to run the object
        /// program, in words, characters or modules. The MEMORY SIZE clause is
        /// syntax checked but has no effect on the execution of the program.
        /// </summary>
        public IntegerValue MemorySize { get; set; }

        /// <summary>
        /// Specifies the amount of main storage needed to run the object
        /// program, in words, characters or modules.
        /// </summary>
        public SyntaxProperty<MemorySizeUnit> MemorySizeUnit { get; set; }

        /// <summary>
        /// The collating sequence used in this program is the collating sequence
        /// associated with the specified alphabet-name.
        /// The collating sequence pertains to this program and to any programs that
        /// this program might contain.
        /// PROGRAM COLLATING SEQUENCE determines the truth value of the
        /// following alphanumeric comparisons:
        /// - Those explicitly specified in relation conditions
        /// - Those explicitly specified in condition-name conditions
        /// The PROGRAM COLLATING SEQUENCE clause also applies to any
        /// merge or sort keys described with usage DISPLAY, unless the COLLATING
        /// SEQUENCE phrase is specified in the MERGE or SORT statement.
        /// The PROGRAM COLLATING SEQUENCE clause does not apply to DBCS
        /// data items or data items of usage NATIONAL.
        /// If the PROGRAM COLLATING SEQUENCE clause is omitted, the EBCDIC
        /// collating sequence is used.
        /// </summary>
        public SymbolReference CollatingSequence { get; set; }

        /// <summary>
        /// The SEGMENT-LIMIT clause is syntax checked but has no effect on the
        /// execution of the program.
        /// All sections with priority-numbers 0 through 49 are fixed permanent segments. 
        /// Segmentation is not supported for programs compiled with the THREAD option.
        /// </summary>
        public IntegerValue SegmentLimit { get; set; }
    }

    /// <summary>
    /// Specifies the amount of main storage needed to run the object
    /// program, in words, characters or modules.
    /// </summary>
    public enum MemorySizeUnit
    {
        Words,
        Characters,
        Modules
    }
}

using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The SOURCE-COMPUTER paragraph describes the computer on which the source
    /// text is to be compiled.
    /// </summary>
    public class SourceComputerParagraph : CodeElement
    {
        public SourceComputerParagraph() : base(CodeElementType.SourceComputerParagraph)
        { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.Unspecified;
        /// <summary>
        /// computer-name
        /// A system-name. For example: IBM-system
        /// </summary>
        public AlphanumericValue ComputerName { get; set; }

        /// <summary>
        /// WITH DEBUGGING MODE
        /// Activates a compile-time switch for debugging lines written in the source
        /// text.
        /// A debugging line is a statement that is compiled only when the
        /// compile-time switch is activated. Debugging lines allow you, for example,
        /// to check the value of a data-name at certain points in a procedure.
        /// To specify a debugging line in your program, code a D in column 7
        /// (indicator area). You can include successive debugging lines, but each must
        /// have a D in column 7, and you cannot break character strings across lines.
        /// All your debugging lines must be written so that the program is
        /// syntactically correct, whether the debugging lines are compiled or treated
        /// as comments.
        /// The presence or absence of the DEBUGGING MODE clause is logically
        /// determined after all COPY and REPLACE statements have been processed.
        /// You can code debugging lines in the ENVIRONMENT DIVISION (after the
        /// OBJECT-COMPUTER paragraph), and in the data and procedure divisions.
        /// </summary>
        public SyntaxProperty<bool> DebuggingMode { get; set; }
    }
}

using System;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// An end program marker separates each program in the sequence of programs. 
    /// program-name must be identical to a program-name declared in a preceding program-ID paragraph.
    /// An end program marker is optional for the last program in the sequence only if that program does not contain any nested source programs.
    /// </summary>
    public class ProgramEnd : CodeElementEnd
    {
        public ProgramEnd() : base(CodeElementType.ProgramEnd)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
        /// <summary>
        /// program-name
        /// A user-defined word or alphanumeric literal, but not a figurative constant,
        /// that identifies your program.
        /// </summary>
        public SymbolReference ProgramName { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder(base.ToString());
            sb.AppendLine("- ProgramName = " + ProgramName);
            return sb.ToString();
        }
    }
}

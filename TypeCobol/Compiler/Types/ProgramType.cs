using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The Type of a Program.
    /// </summary>
    public class ProgramType : Type
    {
        /// <summary>
        /// Empty Constructor
        /// </summary>
        public ProgramType()
            : this(Tags.Program)
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        protected ProgramType(Tags tag)
            : base(tag)
        {
        }

        /// <summary>
        /// Usings variables from a Linkage section
        /// </summary>
        public List<VariableSymbol> Usings
        {
            get;
            set;
        }

        /// <summary>
        /// The return variable if any.
        /// </summary>
        public VariableSymbol ReturnVariable
        {
            get;
            set;
        }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            if (Usings != null && Usings.Count > 0)
            {
                output.Write(indent);
                output.WriteLine($"Usings: [{string.Join(", ", Usings.Select(v => v.FullName))}]");//Write reference
            }

            if (ReturnVariable != null)
            {
                output.Write(indent);
                output.WriteLine($"ReturnVariable: {ReturnVariable.FullName}");//Write reference
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitProgramType(this, arg);
        }
    }
}

using System.Collections.Generic;
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

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitProgramType(this, arg);
        }
    }
}

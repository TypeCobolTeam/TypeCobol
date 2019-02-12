using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The Type of a Program.
    /// </summary>
    public class ProgramType : Type
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public ProgramType()
            : this(Tags.Program)
        {
        }

        /// <summary>
        /// Symbol Constructor
        /// <param name="prgSym">The Program Symbol</param>
        /// </summary>
        public ProgramType(ProgramSymbol prgSym)
            : this(Tags.Program)
        {
            base.Symbol = prgSym;
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
    }
}

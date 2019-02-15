using System;
using System.Collections.Generic;
using System.IO;
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

        /// <summary>
        /// The return variable if any.
        /// </summary>
        public VariableSymbol ReturnVariable
        {
            get;
            set;
        }

        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            if (Usings != null)
            {
                tw.WriteLine();
                tw.Write(s);
                foreach (var p in Usings)
                {
                    if (p.HasFlag(Symbol.Flags.ByValue))
                        tw.Write("BY VALUE ");
                    else if (p.HasFlag(Symbol.Flags.ByReference))
                        tw.Write("BY REFERENCE ");
                    else if (p.HasFlag(Symbol.Flags.ByContent))
                        tw.Write("BY CONTENT ");
                    p.Dump(tw, 0);
                }
            }
            tw.WriteLine();
            ReturnVariable?.Dump(tw, indentLevel);
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitProgramType(this, s); }
    }
}

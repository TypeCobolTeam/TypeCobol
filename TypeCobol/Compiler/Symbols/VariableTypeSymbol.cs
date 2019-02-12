using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A variable declared has having a type that comes from a TypeDef.
    /// Suche variable is expanded to have the expanded type from a TypeDef
    /// </summary>
    public class VariableTypeSymbol : VariableSymbol
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="tdSym">The associated TypeDef symbol</param>
        public VariableTypeSymbol(string name, TypedefSymbol tdSym) : this(name, tdSym, 0)
        {
        }

        /// <summary>
        /// onstructor
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="tdSym">The associated TypeDef symbol</param>
        /// <param name="index">The Variable's Global Index</param>
        internal VariableTypeSymbol(string name, TypedefSymbol tdSym, uint index) : base(name, index)
        {
            System.Diagnostics.Debug.Assert(tdSym != null);
            SetFlag(Flags.HasATypedefType, true);
            Typedef = tdSym;
        }

        /// <summary>
        /// The Typedef symbol
        /// </summary>
        public TypedefSymbol Typedef
        {
            get;
            private set;
        }

        public override bool Check(Func<uint> varSymbolIndex)
        {
            if (Type == null && Typedef.Type != null)
            {//This Typedef symbol is resolved ==> expand our type to this Typedef type
                Type = Typedef.Type.Expand(this, true, varSymbolIndex);
            }
            return Type != null;
        }
    }
}

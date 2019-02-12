using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Represents a Redefines Symbol, for instance in the example Below it is the variable B.
    /// ex:
    /// 05  A PICTURE X(6).
    /// 05  B REDEFINES A.
    ///     10 B-1          PICTURE X(2).
    ///     10 B-2          PICTURE 9(4).
    /// 05  C PICTURE 99V99.
    /// </summary>
    public class RedefinesSymbol : VariableSymbol
    {
        public RedefinesSymbol(string name) : base(name)
        {
            base.SetFlag(Flags.Rededines, true);
        }

        /// <summary>
        /// The redefined symbol
        /// </summary>
        public VariableSymbol Redefined
        {
            get;
            set;
        }
    }
}

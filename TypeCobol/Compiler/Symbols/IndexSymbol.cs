using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents an Index.
    /// The tupe of an index is : S9(8) comp
    /// </summary>
    public class IndexSymbol : VariableSymbol
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        /// <param name="name">The Index name</param>
        public IndexSymbol(String name)
            : this(name, 0)
        {
        }

        /// <summary>
        /// Index Name creator
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="varIndex">Internal Variable Index</param>
        internal IndexSymbol(String name, uint varIndex)
            : base(name, varIndex)
        {
            Kind = Kinds.Index;
            Type = BuiltinTypes.IndexType;
        }

    }
}

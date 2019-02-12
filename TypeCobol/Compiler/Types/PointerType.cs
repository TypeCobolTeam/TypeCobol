using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;
using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The Pointer Type.
    /// </summary>
    public class PointerType : Type
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public PointerType()
            : base(Tags.Pointer)
        {
        }

        /// <summary>
        /// The Type of an Element
        /// </summary>
        public Type ElementType
        {
            get;
            set;
        }

        public override Type Expand(Symbol owner, bool bClone, Func<uint> varSymIndexer)
        {
            if (bClone)
            {
                PointerType ptrType = new PointerType();
                ptrType.ElementType = (Type)ElementType.Expand(owner, bClone, varSymIndexer);
                return ptrType;
            }
            else
            {
                ElementType = (Type)ElementType.Expand(owner, bClone, varSymIndexer);
                return this;
            }
        }

        public override void SetFlag(Flags flag, bool value)
        {
            ElementType.SetFlag(flag, value);
        }
    }
}

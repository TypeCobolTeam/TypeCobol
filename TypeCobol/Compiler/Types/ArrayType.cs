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
    /// Class that represents an Array Type
    /// </summary>
    public class ArrayType : Type
    {
        /// <summary>
        /// Empty Constructor
        /// </summary>
        public ArrayType()
            : base(Tags.Array)
        {
        }

        /// <summary>
        /// The Minimal Occurennce number in the array.
        /// </summary>
        public long MinOccur
        {
            get;
            set;
        }

        /// <summary>
        /// The Maximal Occurennce number in the array.
        /// </summary>
        public long MaxOccur
        {
            get;
            set;
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
                ArrayType arrayType = (ArrayType)MemberwiseClone();
                arrayType.MinOccur = MinOccur;
                arrayType.MaxOccur = MaxOccur;
                arrayType.ElementType = (Type)ElementType.Expand(owner, bClone, varSymIndexer);
                return arrayType;
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

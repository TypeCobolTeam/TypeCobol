using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// A TypeDef type.
    /// </summary>
    public class TypedefType : Type
    {
        /// <summary>
        /// Empty constructor.
        /// </summary>
        public TypedefType()
            : base(Tags.Typedef)
        {
        }

        /// <summary>
        /// Type Constructor
        /// </summary>
        /// <param name="targetType"></param>
        public TypedefType(Type targetType)
            : base(Tags.Typedef)
        {
            System.Diagnostics.Debug.Assert(targetType != null);
            TargetType = targetType;
        }

        /// <summary>
        /// The target type of the Typedef
        /// </summary>
        public Type TargetType
        {
            get;
            set;
        }

        /// <summary>
        /// Expand a TypeDef by create the same type from it by copying it.
        /// </summary>
        /// <param name="owner">Owner which request the expansion</param>
        /// <param name="bClone">Shall we clone by expansion or simply copy</param>
        /// <param name="varSymbolIndex">The Variable Symbol Indexer Function.</param>
        /// <returns></returns>
        public override Type Expand(Symbol owner,  bool bClone, Func<uint> varSymbolIndex)
        {
            return TargetType?.Expand(owner, bClone, varSymbolIndex);
        }
    }
}

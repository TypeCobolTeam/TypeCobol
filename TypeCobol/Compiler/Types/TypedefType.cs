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
    /// A TypeDef type.
    /// </summary>
    public class TypedefType : Type
    {
        /// <summary>
        /// Empty constructor.
        /// <param name="symbol">Typedef's symbol</param>
        /// </summary>
        public TypedefType(TypedefSymbol symbol)
            : base(Tags.Typedef)
        {
            this.Symbol = symbol;
        }

        /// <summary>
        /// Type Constructor
        /// </summary>
        /// <param name="symbol">Typedef's symbol</param>
        /// <param name="targetType">Typedef's type</param>
        public TypedefType(TypedefSymbol symbol, Type targetType)
            : base(Tags.Typedef)
        {
            System.Diagnostics.Debug.Assert(targetType != null);
            TargetType = targetType;
            this.Symbol = symbol;
        }

        /// <summary>
        /// The target type of the Typedef
        /// </summary>
        public Type TargetType
        {
            get;
            set;
        }

        internal override void SetFlag(Symbol.Flags flag, bool value)
        {
            base.SetFlag(flag, value);
            if (TargetType != null)
                TargetType.SetFlag(flag, value);
        }

        /// <summary>
        /// We take the the real representation type.
        /// For example:
        /// T Typedef S-> S Typedef R -> R PIC X(10).
        /// give the type of PIX X(10)
        /// </summary>
        public override Type TypeComponent => TargetType?.TypeComponent ?? TargetType;

        /// <summary>
        /// Typedef may always expand.
        /// </summary>
        public override bool MayExpand => true;

        public override void Dump(TextWriter tw, int indentLevel)
        {
            TargetType?.Dump(tw, indentLevel);
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitTypedefType(this, s); }
    }
}

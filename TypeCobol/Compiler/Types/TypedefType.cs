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
        /// The target type of the Typedef.
        /// For instance in the examples:
        /// 01 Type1 typedef strict private pic X.
        /// 01 Type2 typedef strict private.
        ///    05 Var1 pic X.
        ///
        /// TargetType of Type1 is a PictureType : Pic X
        /// TargetType of Type2 is a  GroupType with one field (05 Var1 pic X)
        /// </summary>
        public Type TargetType
        {
            get;
            set;
        }

        /// <summary>
        /// We take the the real representation type.
        /// For example:
        /// T Typedef S-> S Typedef R -> R PIC X(10).
        /// give the type of PIX X(10)
        /// </summary>
        public override Type TypeComponent => TargetType;

        /// <summary>
        /// Typedef may always expand.
        /// </summary>
        public override bool MayExpand => true;

        public override void Dump(TextWriter tw, int indentLevel)
        {
            //Do not expand typedef while dumping because it may be cyclic.
            tw.Write(Symbol.Name);
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitTypedefType(this, s); }
    }
}

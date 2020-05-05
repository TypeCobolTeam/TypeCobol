using System.IO;
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
        /// The Symbol associated to this type.
        /// </summary>
        public Symbol Symbol
        {
            get;
            set;
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

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            if (Symbol != null)
            {
                output.Write(indent);
                output.WriteLine($"Symbol: {Symbol.FullName}");//Write reference
            }

            if (TargetType != null)
            {
                output.Write(indent);
                output.WriteLine("TargetType:");
                TargetType.Dump(output, indentLevel + 1);
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitTypedefType(this, arg);
        }
    }
}

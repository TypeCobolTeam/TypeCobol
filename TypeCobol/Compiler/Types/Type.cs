using System;
using System.IO;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;

using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// A Cobol Type
    /// </summary>
    public class Type : ISemanticData, ICloneable
    {
        /// <summary>
        /// Type tags, used to quickly determine the Kind of the type, this Type
        /// represents based on a Type instance, and not have to use the C# is operator
        /// for that purpose, so that a static cast can be used if needed, rather
        /// than a dynamic cast.
        /// </summary>
        public enum Tags
        {
            Usage,//This is a type which is only defined by its usage.
            Picture,
            Array,
            Pointer,
            Group,
            Program,
            Function,
            Typedef,
            Renames,

            //TypeCobol Tags
            Boolean,
            String,
        }

        /// <summary>
        /// Usage associated to this type
        /// </summary>
        public enum UsageFormat
        {
            None,           //No associated Usage
            Binary = 1,
            Comp,
            Comp1,
            Comp2,
            Comp3,
            Comp4,
            Comp5,
            Display,
            Display1,
            Index,
            National,
            PackedDecimal,
            ObjectReference,
            Pointer,
            ProcedurePointer,
            FunctionPointer,

            //Cobol Bultin types usage
            Omitted,
            Alphabetic,
            Numeric,
            NumericEdited,
            Alphanumeric,
            AlphanumericEdited,
            DBCS,
            FloatingPoint,
        }

        /// <summary>
        /// Getter on type tag.
        /// </summary>
        public Tags Tag
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="tag">TypeCobol type</param>
        /// <param name="usage">Usage format</param>
        internal Type(Tags tag, UsageFormat usage = UsageFormat.None)
        {
            this.Tag = tag;
            this.Usage = usage;
        }

        /// <summary>
        /// Types's Flags.
        /// </summary>
        public Symbol.Flags Flag
        {
            get;
            internal set;
        }

        public virtual UsageFormat Usage
        {
            get;
            set;
        }

        /// <summary>
        /// The Symbol associated to this type if any: This for a Program or a Function or a TYPEDEF
        /// </summary>
        public Symbol Symbol
        {
            get;
            set;
        }

        /// <summary>
        /// The length of this type.
        /// </summary>
        public virtual int Length
        {
            get
            {
                switch (Usage)
                {
                    case UsageFormat.Comp:
                    case UsageFormat.Comp4:
                    case UsageFormat.Comp5:
                    case UsageFormat.Display1:
                    case UsageFormat.National:
                    case UsageFormat.Binary:
                        return 2;
                    //Floating-point: Specifies for internal floating -point items (single precision)
                    //(i.e float in java, or C)
                    case UsageFormat.Comp1:
                    case UsageFormat.FunctionPointer:
                    case UsageFormat.ObjectReference:
                    case UsageFormat.Index:
                    case UsageFormat.Pointer:
                        return 4;
                    //Long floating-point: Specifies for internal  floating point items(double precision)
                    //(i.e double in java or C)
                    case UsageFormat.Comp2:
                    case UsageFormat.ProcedurePointer:
                        return 8;
                    case UsageFormat.Comp3:
                    case UsageFormat.Display:
                    case UsageFormat.PackedDecimal:
                        return 1;

                    default:
                        throw new ArgumentException("Invalid Usage for type length calculation : " + Usage.ToString());
                }
            }
        }

        public SemanticKinds SemanticKind => SemanticKinds.Type;

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag">Flag or flags to set.</param>
        /// <param name="value">Boolean value indicating whether the flags should be applied or removed.</param>
        /// <param name="propagate">True to apply flags to child components, false otherwise. True is the default for types.</param>
        internal virtual void SetFlag(Flags flag, bool value, bool propagate = true)
        {
            this.Flag = value
                ? (Flags) ((ulong) this.Flag | (ulong) flag)
                : (Flags) ((ulong) this.Flag & ~(ulong) flag);
            if (propagate)
            {
                TypeComponent?.SetFlag(flag, value, true);
            }
        }

        /// <summary>
        /// Determines if the given flag is set.
        /// </summary>
        /// <param name="flag">The flag to be tested</param>
        /// <returns>true if yes, false otherwise.</returns>
        public bool HasFlag(Flags flag)
        {
            return ((ulong)this.Flag & (ulong)flag) != 0;
        }

        public object Clone()
        {
            return MemberwiseClone();
        }

        /// <summary>
        /// TypeComponent for example for Array, Pointer type or TypeDef.
        /// For an array it is the type of an element of the array.
        /// For a pointer it is the type of the pointed element.
        /// For a Typedef it is the type which is defined.
        /// </summary>
        public virtual Type TypeComponent => null;

        /// <summary>
        /// A Type may expand to a Cobol85 if it has a type component.
        /// Or it is a builtin type.
        /// </summary>
        public virtual bool MayExpand => HasFlag(Flags.BuiltinType) || TypeComponent != null;

        public override string ToString()
        {
            StringWriter sw  = new StringWriter();
            Dump(sw, 0);
            return sw.ToString();
        }

        /// <summary>
        /// Dump this type in the given TextWriter instance
        /// </summary>
        /// <param name="tw"></param>
        /// <param name="indentLevel"></param>
        public virtual void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(s);
            tw.Write(System.Enum.GetName(typeof(UsageFormat), Usage));            
        }

        public virtual TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitType(this, s); }

        /// <summary>
        /// A visitor for types.  A visitor is used to implement operations
        /// (or relations) on types. Most common operations on types are
        /// binary relations of the form : Type x TS -> TR 
        /// </summary>
        /// <typeparam name="TR">the return type of the operation implemented by this visitor.
        /// </typeparam>
        /// <typeparam name="TS">the type of the second argument (the first being the
        /// symbol itself) of the operation implemented by this visitor.
        /// </typeparam>
        public interface IVisitor<out TR, in TS>
        {
            TR VisitArrayType(ArrayType t, TS s);
            TR VisitFunctionType(FunctionType t, TS s);
            TR VisitPictureType(PictureType t, TS s);
            TR VisitPointerType(PointerType t, TS s);
            TR VisitProgramType(ProgramType t, TS s);
            TR VisitGroupType(GroupType t, TS s);
            TR VisitRenamesType(RenamesType t, TS s);
            TR VisitTypedefType(TypedefType t, TS s);
            TR VisitType(Type t, TS s);
        }

        /// <summary>
        /// The Abstract Type Visitor Class
        /// </summary>
        /// <typeparam name="TR"></typeparam>
        /// <typeparam name="TS"></typeparam>
        public abstract class AbstractTypeVisitor<TR, TS>  : IVisitor<TR, TS>
        {
            public TR Visit(Type t, TS s) { return t.Accept(this, s); }
            public virtual TR VisitArrayType(ArrayType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitFunctionType(FunctionType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitPictureType(PictureType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitPointerType(PointerType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitProgramType(ProgramType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitGroupType(GroupType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitRenamesType(RenamesType t, TS s) { return VisitType(t, s); }
            public virtual TR VisitTypedefType(TypedefType t, TS s) { return VisitType(t, s); }
            public abstract TR VisitType(Type t, TS s);
        }
    }
}

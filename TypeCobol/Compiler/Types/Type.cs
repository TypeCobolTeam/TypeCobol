using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.CodeElements;

using static TypeCobol.Compiler.Symbols.Symbol;


namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// A Cobol Type
    /// </summary>
    public class Type : ISemanticData, ICloneable
    {
        /// <summary>
        /// Type tags, used to quickly determine the nature of a Type instance.
        /// This avoids use of the C# is operator and a static cast can be used if needed,
        /// instead of a dynamic cast.
        /// </summary>
        public enum Tags
        {
            //Type inheritors
            Array,
            Group,
            Picture,
            Program,
            Function,
            Typedef,

            //Additional tags (those can't be used to downcast !)
            Usage,         //The type is defined only by its usage
            DataCondition, //Level-88 data conditions
            Boolean,       //TypeCobol built-in type Bool
            String         //TypeCobol built-in type String
        }

        /// <summary>
        /// Usage associated to this type
        /// </summary>
        public enum UsageFormat
        {
            None, //No usage associated
            Comp, //Synonym of BINARY and also COMP-4
            Comp1,
            Comp2,
            Comp3, //Synonym of PackedDecimal
            Comp5,
            Display,
            Display1,
            Index,
            National,
            ObjectReference,
            Pointer,
            ProcedurePointer,
            FunctionPointer
        }

        /// <summary>
        /// Convert a data usage to a Type UsageFormat.
        /// </summary>
        /// <param name="usage">Data usage.</param>
        /// <returns>Corresponding instance of UsageFormat.</returns>
        internal static UsageFormat DataUsage2UsageFormat(DataUsage usage)
        {
            switch (usage)
            {
                case DataUsage.Binary:
                    return UsageFormat.Comp;
                case DataUsage.NativeBinary:
                    return UsageFormat.Comp5;
                case DataUsage.PackedDecimal:
                    return UsageFormat.Comp3;
                case DataUsage.FloatingPoint:
                    return UsageFormat.Comp1;
                case DataUsage.LongFloatingPoint:
                    return UsageFormat.Comp2;
                case DataUsage.Display:
                    return UsageFormat.Display;
                case DataUsage.DBCS:
                    return UsageFormat.Display1;
                case DataUsage.FunctionPointer:
                    return UsageFormat.FunctionPointer;
                case DataUsage.Index:
                    return UsageFormat.Index;
                case DataUsage.National:
                    return UsageFormat.National;
                case DataUsage.ObjectReference:
                    return UsageFormat.ObjectReference;
                case DataUsage.Pointer:
                    return UsageFormat.Pointer;
                case DataUsage.ProcedurePointer:
                    return UsageFormat.ProcedurePointer;
                default:
                    return UsageFormat.None;
            }
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
        /// Type's Flags.
        /// </summary>
        public Flags Flag
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
        /// The length of this type.
        /// </summary>
        public virtual int Length
        {
            get
            {
                if(Tag == Tags.Boolean)
                {
                    return 1;
                }
                else if(Tag == Tags.DataCondition)
                {
                    return 0;
                }
                switch (Usage)
                {
                    case UsageFormat.Comp:
                    case UsageFormat.Comp5:
                    case UsageFormat.Display1:
                    case UsageFormat.National:
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
            var output = new StringWriter();
            Dump(output, 0);
            return output.ToString();
        }

        public virtual void Dump(TextWriter output, int indentLevel)
        {
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"(.NET Type={GetType().Name}, Tag={Tag})");
            if (Flag != 0)
            {
                output.Write(indent);
                output.WriteLine($"Flags: [{string.Join(", ", GetFlagsLabels())}]");
            }

            if (Usage != UsageFormat.None)
            {
                output.Write(indent);
                output.WriteLine($"Usage: {Usage}");
            }

            IEnumerable<string> GetFlagsLabels()
            {
                foreach (Flags flag in Enum.GetValues(typeof(Flags)))
                    if (Flag.HasFlag(flag))
                        yield return flag.ToString();
            }
        }

        /// <summary>
        /// Can I move otherType to this ?
        /// </summary>
        /// <returns></returns>
        public virtual bool IsAssignableFrom(Type otherType)
        {
            //TODO
        }

        /// <summary>
        /// Can I call  otherType to this ?
        /// We want almost strictly match between type.
        /// Exceptions : TODO
        /// 
        /// 
        /// This method is not symetric.
        /// 
        /// Note : Order of Type/TypeComponent is guaranteed to be always the same:
        /// Both lines will produce: ArrayType -> GroupType -> Picture)      
        ///     05 group1 occurs 5 pic X.   88 value1 ...
        ///     05 group1 pic X occurs 5 .  88 value1 ...
        /// </summary>
        /// <returns></returns>
        public virtual bool IsEquivalentTo(Type otherType)
        {
            if (otherType==null)
            {
                return false;
            }
            /*
            Array,
            Group,
            Picture,

            Usage,         //The type is defined only by its usage
            DataCondition, //Level-88 data conditions
            Boolean,       //TypeCobol built-in type Bool
            */
            switch(this.Tag)
            {
                case Tags.Usage:
                    return this.Tag == otherType.Tag && this.Usage == otherType.Usage;
                case Tags.DataCondition:
                case Tags.Boolean:
                    return this.Tag == otherType.Tag;
                default:
                    System.Diagnostics.Debug.Fail("Root class Type cannot have usage " + this.Tag);
                    return false;
            }

            //Don't compare ComponentType because it's always null for this root class
        }

        public virtual TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitType(this, arg);
        }

        /// <summary>
        /// A visitor for types.  A visitor is used to implement operations
        /// (or relations) on types. Most common operations on types are
        /// binary relations of the form : Type x TParameter -> TResult
        /// </summary>
        /// <typeparam name="TResult">the return type of the operation implemented by this visitor.
        /// </typeparam>
        /// <typeparam name="TParameter">the type of the second argument (the first being the
        /// type itself) of the operation implemented by this visitor.
        /// </typeparam>
        public interface IVisitor<out TResult, in TParameter>
        {
            TResult VisitArrayType(ArrayType t, TParameter arg);
            TResult VisitFunctionType(FunctionType t, TParameter arg);
            TResult VisitPictureType(PictureType t, TParameter arg);
            TResult VisitProgramType(ProgramType t, TParameter arg);
            TResult VisitGroupType(GroupType t, TParameter arg);
            TResult VisitTypedefType(TypedefType t, TParameter arg);
            TResult VisitType(Type t, TParameter arg);
        }

        /// <summary>
        /// The Abstract Type Visitor Class
        /// </summary>
        /// <typeparam name="TResult">Result type of the visitor.</typeparam>
        /// <typeparam name="TParameter">Parameter type of the visitor.</typeparam>
        public abstract class AbstractTypeVisitor<TResult, TParameter>  : IVisitor<TResult, TParameter>
        {
            public TResult Visit(Type t, TParameter arg) { return t.Accept(this, arg); }
            public virtual TResult VisitArrayType(ArrayType t, TParameter arg) { return VisitType(t, arg); }
            public virtual TResult VisitFunctionType(FunctionType t, TParameter arg) { return VisitType(t, arg); }
            public virtual TResult VisitPictureType(PictureType t, TParameter arg) { return VisitType(t, arg); }
            public virtual TResult VisitProgramType(ProgramType t, TParameter arg) { return VisitType(t, arg); }
            public virtual TResult VisitGroupType(GroupType t, TParameter arg) { return VisitType(t, arg); }
            public virtual TResult VisitTypedefType(TypedefType t, TParameter arg) { return VisitType(t, arg); }
            public abstract TResult VisitType(Type t, TParameter arg);
        }
    }
}

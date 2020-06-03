using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;
using static TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Some Builtin Types
    /// </summary>
    public class BuiltinTypes
    {
        public static readonly Type Comp1Type;
        public static readonly Type Comp2Type;
        public static readonly Type FloatType;
        public static readonly Type DoubleType;
        public static readonly Type PointerType;
        public static readonly Type FunctionPointerType;
        public static readonly Type ProcedurePointerType;
        public static readonly Type IndexType;
        public static readonly Type ObjectReferenceType;

        public static readonly Type OmittedType;
        public static readonly Type AlphabeticType;
        public static readonly Type NumericType;
        public static readonly Type NumericEditedType;
        public static readonly Type AlphanumericType;
        public static readonly Type AlphanumericEditedType;
        public static readonly Type DBCSType;
        public static readonly Type FloatingPointType;

        public static readonly Type BooleanType;
        public static readonly Type DateType;
        public static readonly Type CurrencyType;
        public static readonly Type StringType;


        #region BuiltinUsageTypes
        //Built-in Basic usage types.
        public readonly static Type UsageNoType = new Type(Tags.Usage, UsageFormat.None);
        public readonly static Type UsageCompType = new Type(Tags.Usage, UsageFormat.Comp);
        public readonly static Type UsageComp4Type = new Type(Tags.Usage, UsageFormat.Comp4);
        public readonly static Type UsageComp5Type = new Type(Tags.Usage, UsageFormat.Comp5);
        public readonly static Type UsageDisplay1Type = new Type(Tags.Usage, UsageFormat.Display1);
        public readonly static Type UsageNationalType = new Type(Tags.Usage, UsageFormat.National);
        public readonly static Type UsageBinaryType = new Type(Tags.Usage, UsageFormat.Binary);
        public readonly static Type UsageComp1Type = new Type(Tags.Usage, UsageFormat.Comp1);
        public readonly static Type UsageFunctionPointerType = new Type(Tags.Usage, UsageFormat.FunctionPointer);
        public readonly static Type UsageObjectReferenceType = new Type(Tags.Usage, UsageFormat.ObjectReference);
        public readonly static Type UsageIndexType = new Type(Tags.Usage, UsageFormat.Index);
        public readonly static Type UsagePointerType = new Type(Tags.Usage, UsageFormat.Pointer);
        public readonly static Type UsageComp2Type = new Type(Tags.Usage, UsageFormat.Comp2);
        public readonly static Type UsageDisplayType = new Type(Tags.Usage, UsageFormat.Display);
        public readonly static Type UsagePackedDecimalType = new Type(Tags.Usage, UsageFormat.PackedDecimal);
        public readonly static Type UsageProcedurePointerType = new Type(Tags.Usage, UsageFormat.ProcedurePointer);
        public readonly static Type UsageComp3Type = new Type(Tags.Usage, UsageFormat.Comp3);
        #endregion


        /// <summary>
        /// Static constructor
        /// </summary>
        static BuiltinTypes()
        {
            Comp1Type = UsageComp1Type;
            FloatType = Comp1Type;
            FloatType.SetFlag(Symbol.Flags.BuiltinType, true);
            Comp2Type = UsageComp2Type;
            DoubleType = Comp2Type;
            DoubleType.SetFlag(Symbol.Flags.BuiltinType, true);
            PointerType = UsagePointerType;
            PointerType.SetFlag(Symbol.Flags.BuiltinType, true);
            FunctionPointerType = UsageFunctionPointerType;
            FunctionPointerType.SetFlag(Symbol.Flags.BuiltinType, true);
            ProcedurePointerType = UsageProcedurePointerType;
            ProcedurePointerType.SetFlag(Symbol.Flags.BuiltinType, true);
            IndexType = UsageIndexType;
            IndexType.SetFlag(Symbol.Flags.BuiltinType, true);
            ObjectReferenceType = UsageObjectReferenceType;
            ObjectReferenceType.SetFlag(Symbol.Flags.BuiltinType, true);

            OmittedType = new Type(Type.Tags.Usage, Type.UsageFormat.Omitted);
            OmittedType.SetFlag(Symbol.Flags.BuiltinType, true);
            AlphabeticType = new Type(Type.Tags.Usage, Type.UsageFormat.Alphabetic);
            AlphabeticType.SetFlag(Symbol.Flags.BuiltinType, true);
            NumericType = new Type(Type.Tags.Usage, Type.UsageFormat.Numeric);
            NumericType.SetFlag(Symbol.Flags.BuiltinType, true);
            NumericEditedType = new Type(Type.Tags.Usage, Type.UsageFormat.NumericEdited);
            NumericEditedType.SetFlag(Symbol.Flags.BuiltinType, true);
            AlphanumericType = new Type(Type.Tags.Usage, Type.UsageFormat.Alphanumeric);
            AlphanumericType.SetFlag(Symbol.Flags.BuiltinType, true);
            AlphanumericEditedType = new Type(Type.Tags.Usage, Type.UsageFormat.AlphanumericEdited);
            AlphanumericEditedType.SetFlag(Symbol.Flags.BuiltinType, true);
            DBCSType = new Type(Type.Tags.Usage, Type.UsageFormat.DBCS);
            DBCSType.SetFlag(Symbol.Flags.BuiltinType, true);
            FloatingPointType = new Type(Type.Tags.Usage, Type.UsageFormat.FloatingPoint);
            FloatingPointType.SetFlag(Symbol.Flags.BuiltinType, true);

            BooleanType = new Type(Type.Tags.Boolean);
            BooleanType.SetFlag(Symbol.Flags.BuiltinType, true);

            DateType = BuiltinTypes.CreateDateType(new TypedefSymbol(string.Intern("Date")));
            DateType.SetFlag(Symbol.Flags.BuiltinType, true);

            CurrencyType = BuiltinTypes.CreateCurrencyType(new TypedefSymbol(string.Intern("Currency")));
            CurrencyType.SetFlag(Symbol.Flags.BuiltinType, true);

            StringType = new Type(Type.Tags.String);
            StringType.SetFlag(Symbol.Flags.BuiltinType, true);
        }

    /// <summary>
    /// Get the TypeCobol Builtin type corresponding to a single usage.
    /// </summary>
    /// <param name="usage">The usage</param>
    /// <returns>The Type instance corresponding to the usage.</returns>
    public static Type BuiltinUsageType(UsageFormat usage)
        {
            switch (usage)
            {
                case UsageFormat.None:
                    return UsageNoType;
                case UsageFormat.Comp:
                    return UsageCompType;
                case UsageFormat.Comp4:
                    return UsageComp4Type;
                case UsageFormat.Comp5:
                    return UsageComp5Type;
                case UsageFormat.Display1:
                    return UsageDisplay1Type;
                case UsageFormat.National:
                    return UsageNationalType;
                case UsageFormat.Binary:
                    return UsageBinaryType;
                //Floating-point: Specifies for internal floating -point items (single precision)
                //(i.e float in java, or C)
                case UsageFormat.Comp1:
                    return UsageComp1Type;
                case UsageFormat.FunctionPointer:
                    return UsageFunctionPointerType;
                case UsageFormat.ObjectReference:
                    return UsageObjectReferenceType;
                case UsageFormat.Index:
                    return UsageIndexType;
                case UsageFormat.Pointer:
                    return UsagePointerType;
                //Long floating-point: Specifies for internal  floating point items(double precision)
                //(i.e double in java or C)
                case UsageFormat.Comp2:
                    return UsageComp2Type;
                case UsageFormat.ProcedurePointer:
                    return UsageProcedurePointerType;
                case UsageFormat.Comp3:
                    return UsageComp3Type;
                case UsageFormat.Display:
                    return UsageDisplayType;
                case UsageFormat.PackedDecimal:
                    return UsagePackedDecimalType;
                default:
                    throw new ArgumentException("Invalid Usage : " + usage.ToString());
            }
        }

        public static Type DateYYYYType = new PictureType(new PictureValidator("9(04)", false));
        public static Type DateMMType = new PictureType(new PictureValidator("9(02)", false));
        public static Type DateDDType = DateMMType;
        public static Type CY_pic = new PictureType(new PictureValidator("X(03)", false));


        /// <summary>
        /// Create the Date Type.
        /// </summary>
        /// <param name="symbol">The Typedef symbol to be associated to the Date type</param>
        /// <returns>The Date type</returns>
        internal static Type CreateDateType(TypedefSymbol symbol)
        {
            symbol.Level = 1;
            GroupType recType = new GroupType(symbol);
            VariableSymbol yyyy = new VariableSymbol("YYYY") { Level = 2, Type = DateYYYYType, Owner = symbol };
            recType.Fields.Enter(yyyy);

            VariableSymbol mm = new VariableSymbol("MM") { Level = 2, Type = DateMMType, Owner = symbol };
            recType.Fields.Enter(mm);

            VariableSymbol dd = new VariableSymbol("DD") { Level = 2, Type = DateDDType, Owner = symbol };
            recType.Fields.Enter(dd);

            TypedefType dateType = new TypedefType(symbol, recType);            
            symbol.Type = dateType;
            //IMPORTANT Mark all symbol has belonging to a TYPEDEF, in order that the expander works.
            symbol.SetFlag(Symbol.Flags.InsideTypedef, true, true);
            return dateType;
        }

        /// <summary>
        /// Create the Currency Type.
        /// </summary>
        /// <param name="symbol">The Typedef symbol to be associated to the Currency type</param>
        /// <returns>The Currency type</returns>
        internal static Type CreateCurrencyType(TypedefSymbol symbol)
        {
            symbol.Level = 1;
            TypedefType currencyType = new TypedefType(symbol, CY_pic);
            symbol.Type = currencyType;
            //IMPORTANT Mark all symbol has belonging to a TYPEDEF, in order that the expander works.
            symbol.SetFlag(Symbol.Flags.InsideTypedef, true, true);
            return currencyType;
        }
    }
}

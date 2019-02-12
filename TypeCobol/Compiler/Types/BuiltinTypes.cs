using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        public static readonly Type OccursType;

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
            Comp1Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp1);
            FloatType = Comp1Type;
            Comp2Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp2);
            DoubleType = Comp2Type;
            PointerType = new Type(Type.Tags.Usage, Type.UsageFormat.Pointer);
            FunctionPointerType = new Type(Type.Tags.Usage, Type.UsageFormat.FunctionPointer);
            ProcedurePointerType = new Type(Type.Tags.Usage, Type.UsageFormat.ProcedurePointer);
            IndexType = new Type(Type.Tags.Usage, Type.UsageFormat.Index);
            ObjectReferenceType = new Type(Type.Tags.Usage, Type.UsageFormat.ObjectReference);

            OmittedType = new Type(Type.Tags.Usage, Type.UsageFormat.Omitted);
            AlphabeticType = new Type(Type.Tags.Usage, Type.UsageFormat.Alphabetic);
            NumericType = new Type(Type.Tags.Usage, Type.UsageFormat.Numeric);
            NumericEditedType = new Type(Type.Tags.Usage, Type.UsageFormat.NumericEdited);
            AlphanumericType = new Type(Type.Tags.Usage, Type.UsageFormat.Alphanumeric);
            AlphanumericEditedType = new Type(Type.Tags.Usage, Type.UsageFormat.AlphanumericEdited);
            DBCSType = new Type(Type.Tags.Usage, Type.UsageFormat.DBCS);
            FloatingPointType = new Type(Type.Tags.Usage, Type.UsageFormat.FloatingPoint);
            OccursType = new Type(Type.Tags.Usage, Type.UsageFormat.Occurs);

            BooleanType = new Type(Type.Tags.Usage, Type.UsageFormat.Boolean);
            DateType = new Type(Type.Tags.Usage, Type.UsageFormat.Date);
            CurrencyType = new Type(Type.Tags.Usage, Type.UsageFormat.Currency);
            StringType = new Type(Type.Tags.Usage, Type.UsageFormat.String);
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

    }
}

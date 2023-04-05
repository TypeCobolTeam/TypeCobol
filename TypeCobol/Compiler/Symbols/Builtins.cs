using TypeCobol.Compiler.Types;

using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// All builtin Symbols
    /// </summary>
    public static class Builtins
    {
        //Builtin types

        //Types only defined by their usage.
        public static readonly Type NoType;
        public static readonly Type CompType;
        public static readonly Type Comp1Type;
        public static readonly Type Comp2Type;
        public static readonly Type Comp3Type;
        public static readonly Type Comp5Type;
        public static readonly Type DisplayType;
        public static readonly Type Display1Type;
        public static readonly Type IndexType;
        public static readonly Type NationalType;
        public static readonly Type ObjectReferenceType;
        public static readonly Type PointerType;
        public static readonly Type ProcedurePointerType;
        public static readonly Type FunctionPointerType;

        //DataCondition / level-88
        public static readonly Type DataConditionType;

        //Builtin symbols
        public static readonly TypedefSymbol Boolean;
        public static readonly TypedefSymbol Date;
        public static readonly TypedefSymbol Currency;
        public static readonly TypedefSymbol String;

        /// <summary>
        /// Static constructor.
        /// </summary>
        static Builtins()
        {
            NoType = new Type(Type.Tags.Usage);
            CompType = new Type(Type.Tags.Usage, Type.UsageFormat.Comp);
            Comp1Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp1);
            Comp2Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp2);
            Comp3Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp3);
            Comp5Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp5);
            DisplayType = new Type(Type.Tags.Usage, Type.UsageFormat.Display);
            Display1Type = new Type(Type.Tags.Usage, Type.UsageFormat.Display1);
            IndexType = new Type(Type.Tags.Usage, Type.UsageFormat.Index);
            NationalType = new Type(Type.Tags.Usage, Type.UsageFormat.National);
            ObjectReferenceType = new Type(Type.Tags.Usage, Type.UsageFormat.ObjectReference);
            PointerType = new Type(Type.Tags.Usage, Type.UsageFormat.Pointer);
            ProcedurePointerType = new Type(Type.Tags.Usage, Type.UsageFormat.ProcedurePointer);
            FunctionPointerType = new Type(Type.Tags.Usage, Type.UsageFormat.FunctionPointer);

            DataConditionType = new Type(Type.Tags.DataCondition);

            Boolean = new TypedefSymbol(string.Intern("Bool"));
            Boolean.Type = new TypedefType(Boolean, new Type(Type.Tags.Boolean));
            FlagSymbol(Boolean);

            Date = new TypedefSymbol(string.Intern("Date"));
            Date.Type = new TypedefType(Date, CreateDateComponent());
            FlagSymbol(Date);

            Currency = new TypedefSymbol(string.Intern("Currency"));
            Currency.Type = new TypedefType(Currency, CreateCurrencyComponent());
            FlagSymbol(Currency);

            String = new TypedefSymbol(string.Intern("String"));
            String.Type = new TypedefType(String, new Type(Type.Tags.String));
            FlagSymbol(String);

            void FlagSymbol(TypedefSymbol typedef)
            {
                typedef.SetFlag(Symbol.Flags.Strong, true);
                typedef.SetFlag(Symbol.Flags.BuiltinSymbol, true);
                typedef.SetFlag(Symbol.Flags.InsideTypedef, true, true);
            }

            GroupType CreateDateComponent()
            {
                var yyyyType = new PictureType(NumericValidationResult(4), false);
                var mmType = new PictureType(NumericValidationResult(3), false);
                var ddType = mmType;

                Date.Level = 1;
                GroupType recType = new GroupType(Date);
                VariableSymbol yyyy = new VariableSymbol("YYYY") { Level = 2, Type = yyyyType };
                recType.Fields.Enter(yyyy);
                VariableSymbol mm = new VariableSymbol("MM") { Level = 2, Type = mmType };
                recType.Fields.Enter(mm);
                VariableSymbol dd = new VariableSymbol("DD") { Level = 2, Type = ddType };
                recType.Fields.Enter(dd);

                return recType;

                PictureValidator.Result NumericValidationResult(int count)
                {
                    return new PictureValidator.Result(
                        new[] { new Character(SC.NINE, count) },
                        null,
                        PictureCategory.Numeric,
                        count,
                        count,
                        false,
                        0,
                        count);
                }
            }

            PictureType CreateCurrencyComponent()
            {
                var validationResult = new PictureValidator.Result(
                    new [] { new Character(SC.X, 3) },
                    null,
                    PictureCategory.Alphanumeric,
                    0,
                    0,
                    false,
                    0,
                    3);
                return new PictureType(validationResult, false);
            }
        }

        internal static Type GetUsageType(Type.UsageFormat usage)
        {
            switch (usage)
            {
                case Type.UsageFormat.None:
                    return NoType;
                case Type.UsageFormat.Comp:
                    return CompType;
                case Type.UsageFormat.Comp1:
                    return Comp1Type;
                case Type.UsageFormat.Comp2:
                    return Comp2Type;
                case Type.UsageFormat.Comp3:
                    return Comp3Type;
                case Type.UsageFormat.Comp5:
                    return Comp5Type;
                case Type.UsageFormat.Display:
                    return DisplayType;
                case Type.UsageFormat.Display1:
                    return Display1Type;
                case Type.UsageFormat.Index:
                    return IndexType;
                case Type.UsageFormat.National:
                    return NationalType;
                case Type.UsageFormat.ObjectReference:
                    return ObjectReferenceType;
                case Type.UsageFormat.Pointer:
                    return PointerType;
                case Type.UsageFormat.ProcedurePointer:
                    return ProcedurePointerType;
                case Type.UsageFormat.FunctionPointer:
                    return FunctionPointerType;
                default:
                    throw new System.ArgumentException("Invalid Usage : " + usage);
            }
        }
    }
}

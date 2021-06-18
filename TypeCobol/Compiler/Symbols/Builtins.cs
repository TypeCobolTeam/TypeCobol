using TypeCobol.Compiler.Types;

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
            NoType = new Type(Type.Tags.Usage, Type.UsageFormat.None); //No need to flag this one because it represents the absence of type.
            FlagType(CompType = new Type(Type.Tags.Usage, Type.UsageFormat.Comp));
            FlagType(Comp1Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp1));
            FlagType(Comp2Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp2));
            FlagType(Comp3Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp3));
            FlagType(Comp5Type = new Type(Type.Tags.Usage, Type.UsageFormat.Comp5));
            FlagType(DisplayType = new Type(Type.Tags.Usage, Type.UsageFormat.Display));
            FlagType(Display1Type = new Type(Type.Tags.Usage, Type.UsageFormat.Display1));
            FlagType(IndexType = new Type(Type.Tags.Usage, Type.UsageFormat.Index));
            FlagType(NationalType = new Type(Type.Tags.Usage, Type.UsageFormat.National));
            FlagType(ObjectReferenceType = new Type(Type.Tags.Usage, Type.UsageFormat.ObjectReference));
            FlagType(PointerType = new Type(Type.Tags.Usage, Type.UsageFormat.Pointer));
            FlagType(ProcedurePointerType = new Type(Type.Tags.Usage, Type.UsageFormat.ProcedurePointer));
            FlagType(FunctionPointerType = new Type(Type.Tags.Usage, Type.UsageFormat.FunctionPointer));

            FlagType(DataConditionType = new Type(Type.Tags.DataCondition));

            Boolean = new TypedefSymbol(string.Intern("Bool"));
            Boolean.Type = new TypedefType(Boolean, new Type(Type.Tags.Boolean));
            Boolean.Type.SetFlag(Symbol.Flags.Strong, true, false);
            FlagSymbol(Boolean);

            Date = new TypedefSymbol(string.Intern("Date"));
            Date.Type = new TypedefType(Date, CreateDateComponent());
            Date.Type.SetFlag(Symbol.Flags.Strong, true, false);
            FlagSymbol(Date);

            Currency = new TypedefSymbol(string.Intern("Currency"));
            Currency.Type = new TypedefType(Currency, CreateCurrencyComponent());
            Currency.Type.SetFlag(Symbol.Flags.Strong, true, false);
            FlagSymbol(Currency);

            String = new TypedefSymbol(string.Intern("String"));
            String.Type = new TypedefType(String, new Type(Type.Tags.String));
            String.Type.SetFlag(Symbol.Flags.Strong, true, false);
            FlagSymbol(String);

            void FlagType(Type type) => type.SetFlag(Symbol.Flags.BuiltinType, true);

            void FlagSymbol(TypedefSymbol typedef)
            {
                FlagType(typedef.Type);
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
                        new[] { new PictureValidator.Character(PictureValidator.SC.NINE, count) },
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
                    new [] { new PictureValidator.Character(PictureValidator.SC.X, 3) },
                    PictureCategory.AlphaNumeric,
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

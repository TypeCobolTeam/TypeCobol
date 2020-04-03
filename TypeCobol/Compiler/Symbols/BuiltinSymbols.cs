using System.Collections.Generic;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// All builtin Symbols
    /// </summary>
    public static class BuiltinSymbols
    {
        //--------------------------
        // Type Symbols
        //--------------------------
        public static readonly TypedefSymbol Omitted;
        public static readonly TypedefSymbol Alphabetic;
        public static readonly TypedefSymbol Numeric;
        public static readonly TypedefSymbol NumericEdited;
        public static readonly TypedefSymbol Alphanumeric;
        public static readonly TypedefSymbol AlphanumericEdited;
        public static readonly TypedefSymbol DBCS;
        public static readonly TypedefSymbol FloatingPoint;

        public static readonly TypedefSymbol Boolean;
        public static readonly TypedefSymbol Date;
        public static readonly TypedefSymbol Currency;
        public static readonly TypedefSymbol String;

        /// <summary>
        /// Static constructor.
        /// </summary>
        static BuiltinSymbols()
        {
            Omitted = new TypedefSymbol(string.Intern("Omitted"));
            Omitted.Type = new TypedefType(Omitted, BuiltinTypes.OmittedType);
            Omitted.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            Alphabetic = new TypedefSymbol(string.Intern("Alphabetic"));
            Alphabetic.Type = new TypedefType(Alphabetic, BuiltinTypes.AlphabeticType);
            Alphabetic.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            Numeric = new TypedefSymbol(string.Intern("Numeric"));
            Numeric.Type = new TypedefType(Numeric, BuiltinTypes.NumericType);
            Numeric.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            NumericEdited = new TypedefSymbol(string.Intern("NumericEdited"));
            NumericEdited.Type = new TypedefType(NumericEdited, BuiltinTypes.NumericEditedType);
            NumericEdited.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            Alphanumeric = new TypedefSymbol(string.Intern("Alphanumeric"));
            Alphanumeric.Type = new TypedefType(Alphanumeric, BuiltinTypes.AlphanumericType);
            Alphanumeric.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            AlphanumericEdited = new TypedefSymbol(string.Intern("AlphanumericEdited"));
            AlphanumericEdited.Type = new TypedefType(AlphanumericEdited, BuiltinTypes.AlphanumericEditedType);
            AlphanumericEdited.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            DBCS = new TypedefSymbol(string.Intern("DBCS"));
            DBCS.Type = new TypedefType(DBCS, BuiltinTypes.DBCSType);
            DBCS.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            FloatingPoint = new TypedefSymbol(string.Intern("FloatingPoint"));
            FloatingPoint.Type = new TypedefType(FloatingPoint, BuiltinTypes.FloatingPointType);
            FloatingPoint.SetFlag(Symbol.Flags.BuiltinSymbol, true);

            Boolean = new TypedefSymbol(string.Intern("Bool"));
            Boolean.Type = new TypedefType(Boolean, BuiltinTypes.BooleanType);
            Boolean.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            Date = (TypedefSymbol)BuiltinTypes.DateType.Symbol;
            Date.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            Currency = (TypedefSymbol)BuiltinTypes.CurrencyType.Symbol;
            Currency.SetFlag(Symbol.Flags.BuiltinSymbol, true);
            String = new TypedefSymbol(string.Intern("String"));
            String.Type = new TypedefType(String, BuiltinTypes.StringType);
            String.SetFlag(Symbol.Flags.BuiltinSymbol, true);
        }

        public static IEnumerable<TypedefSymbol> All
        {
            get
            {
                yield return Omitted;
                yield return Alphabetic;
                yield return Numeric;
                yield return NumericEdited;
                yield return Alphanumeric;
                yield return AlphanumericEdited;
                yield return DBCS;
                yield return FloatingPoint;

                yield return Boolean;
                yield return Date;
                yield return Currency;
                yield return String;
            }
        }
    }
}

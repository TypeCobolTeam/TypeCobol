using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.Scopes;
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
        public static TypedefSymbol Omitted;
        public static TypedefSymbol Alphabetic;
        public static TypedefSymbol Numeric;
        public static TypedefSymbol NumericEdited;
        public static TypedefSymbol Alphanumeric;
        public static TypedefSymbol AlphanumericEdited;
        public static TypedefSymbol DBCS;
        public static TypedefSymbol FloatingPoint;

        public static TypedefSymbol Boolean;
        public static TypedefSymbol Date;
        public static TypedefSymbol Currency;
        public static TypedefSymbol String;

        /// <summary>
        /// Static constructor.
        /// </summary>
        static BuiltinSymbols()
        {
            Omitted = new TypedefSymbol(string.Intern("Omitted"));
            Omitted.Type = new TypedefType(Omitted, BuiltinTypes.OmittedType);
            Alphabetic = new TypedefSymbol(string.Intern("Alphabetic"));
            Alphabetic.Type = new TypedefType(Alphabetic, BuiltinTypes.AlphabeticType);
            Numeric = new TypedefSymbol(string.Intern("Numeric"));
            Numeric.Type = new TypedefType(Numeric, BuiltinTypes.NumericType);
            NumericEdited = new TypedefSymbol(string.Intern("NumericEdited"));
            NumericEdited.Type = new TypedefType(NumericEdited, BuiltinTypes.NumericEditedType);
            Alphanumeric = new TypedefSymbol(string.Intern("Alphanumeric"));
            Alphanumeric.Type = new TypedefType(Alphanumeric, BuiltinTypes.AlphanumericType);
            AlphanumericEdited = new TypedefSymbol(string.Intern("AlphanumericEdited"));
            AlphanumericEdited.Type = new TypedefType(AlphanumericEdited, BuiltinTypes.AlphanumericEditedType);
            DBCS = new TypedefSymbol(string.Intern("DBCS"));
            DBCS.Type = new TypedefType(DBCS, BuiltinTypes.DBCSType);
            FloatingPoint = new TypedefSymbol(string.Intern("FloatingPoint"));
            FloatingPoint.Type = new TypedefType(FloatingPoint, BuiltinTypes.FloatingPointType);

            Boolean = new TypedefSymbol(string.Intern("Bool"));
            Boolean.Type = new TypedefType(Boolean, BuiltinTypes.BooleanType);
            Date = (TypedefSymbol)BuiltinTypes.DateType.Symbol;
            Currency = (TypedefSymbol)BuiltinTypes.CurrencyType.Symbol;
            String = new TypedefSymbol(string.Intern("String"));
            String.Type = new TypedefType(String, BuiltinTypes.StringType);
        }

        /// <summary>
        /// Store Builtins Symbol in the given scope
        /// </summary>
        internal static void StoreSymbols(Scope<TypedefSymbol> types)
        {
            types.Enter(Omitted);
            types.Enter(Alphabetic);
            types.Enter(Numeric);
            types.Enter(NumericEdited);
            types.Enter(Alphanumeric);
            types.Enter(AlphanumericEdited);
            types.Enter(DBCS);
            types.Enter(FloatingPoint);

            types.Enter(Boolean);
            types.Enter(Date);
            types.Enter(Currency);
            types.Enter(String);
        }
    }
}
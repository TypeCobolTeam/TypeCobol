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
        public static TypedefSymbol Occurs;

        public static TypedefSymbol Boolean;
        public static TypedefSymbol Date;
        public static TypedefSymbol Currency;
        public static TypedefSymbol String;

        /// <summary>
        /// Static constructor.
        /// </summary>
        static BuiltinSymbols()
        {
            Omitted = new TypedefSymbol("Omitted");
            Omitted.Type = new TypedefType(BuiltinTypes.OmittedType);
            Alphabetic = new TypedefSymbol("Alphabetic");
            Alphabetic.Type = new TypedefType(BuiltinTypes.AlphabeticType);
            Numeric = new TypedefSymbol("Numeric");
            Numeric.Type = new TypedefType(BuiltinTypes.NumericType);
            NumericEdited = new TypedefSymbol("NumericEdited");
            NumericEdited.Type = new TypedefType(BuiltinTypes.NumericEditedType);
            Alphanumeric = new TypedefSymbol("Alphanumeric");
            Alphanumeric.Type = new TypedefType(BuiltinTypes.AlphanumericType);
            AlphanumericEdited = new TypedefSymbol("AlphanumericEdited");
            AlphanumericEdited.Type = new TypedefType(BuiltinTypes.AlphanumericEditedType);
            DBCS = new TypedefSymbol("DBCS");
            DBCS.Type = new TypedefType(BuiltinTypes.DBCSType);
            FloatingPoint = new TypedefSymbol("FloatingPoint");
            FloatingPoint.Type = new TypedefType(BuiltinTypes.FloatingPointType);
            Occurs = new TypedefSymbol("Array");
            Occurs.Type = new TypedefType(BuiltinTypes.OccursType);


            Boolean = new TypedefSymbol("Bool");
            Boolean.Type = new TypedefType(BuiltinTypes.BooleanType);
            Date = new TypedefSymbol("Date");
            Date.Type = new TypedefType(BuiltinTypes.DateType);
            Currency = new TypedefSymbol("Currency");
            Currency.Type = new TypedefType(BuiltinTypes.CurrencyType);
            String = new TypedefSymbol("String");
            String.Type = new TypedefType(BuiltinTypes.StringType);
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
            types.Enter(Occurs);

            types.Enter(Boolean);
            types.Enter(Date);
            types.Enter(Currency);
            types.Enter(String);
        }
    }
}
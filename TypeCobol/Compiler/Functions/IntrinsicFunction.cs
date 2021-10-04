using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Functions
{
    public class IntrinsicFunction : SymbolDefinition
    {
        private static readonly Dictionary<string, IntrinsicFunction> _All;

        static IntrinsicFunction()
        {
            _All = new Dictionary<string, IntrinsicFunction>(StringComparer.OrdinalIgnoreCase)
                   {
                       { "ACOS", new IntrinsicFunction("ACOS", FunctionType.Numeric) },
                       { "ANNUITY", new IntrinsicFunction("ANNUITY", FunctionType.Numeric) },
                       { "ASIN", new IntrinsicFunction("ASIN", FunctionType.Numeric) },
                       { "ATAN", new IntrinsicFunction("ATAN", FunctionType.Numeric) },
                       { "CHAR", new IntrinsicFunction("CHAR", FunctionType.Alphanumeric) },
                       { "COS", new IntrinsicFunction("COS", FunctionType.Numeric) },
                       { "CURRENT-DATE", new IntrinsicFunction("CURRENT-DATE", FunctionType.Alphanumeric) },
                       { "DATE-OF-INTEGER", new IntrinsicFunction("DATE-OF-INTEGER", FunctionType.Integer) },
                       { "DATE-TO-YYYYMMDD", new IntrinsicFunction("DATE-TO-YYYYMMDD", FunctionType.Integer) },
                       { "DAY-OF-INTEGER", new IntrinsicFunction("DAY-OF-INTEGER", FunctionType.Integer) },
                       { "DAY-TO-YYYYDDD", new IntrinsicFunction("DAY-TO-YYYYDDD", FunctionType.Integer) },
                       { "DISPLAY-OF", new IntrinsicFunction("DISPLAY-OF", FunctionType.Alphanumeric) },
                       { "FACTORIAL", new IntrinsicFunction("FACTORIAL", FunctionType.Integer) },
                       { "INTEGER", new IntrinsicFunction("INTEGER", FunctionType.Integer) },
                       { "INTEGER-OF-DATE", new IntrinsicFunction("INTEGER-OF-DATE", FunctionType.Integer) },
                       { "INTEGER-OF-DAY", new IntrinsicFunction("INTEGER-OF-DAY", FunctionType.Integer) },
                       { "INTEGER-PART", new IntrinsicFunction("INTEGER-PART", FunctionType.Integer) },
                       { "LENGTH", new IntrinsicFunction("LENGTH", FunctionType.Integer) },
                       { "LOG", new IntrinsicFunction("LOG", FunctionType.Numeric) },
                       { "LOG10", new IntrinsicFunction("LOG10", FunctionType.Numeric) },
                       { "LOWER-CASE", new IntrinsicFunction("LOWER-CASE", FunctionType.Alphanumeric) }, //Could be National depending on argument type
                       { "MAX", new IntrinsicFunction("MAX", FunctionType.Numeric) }, //Depends on arguments
                       { "MEAN", new IntrinsicFunction("MEAN", FunctionType.Numeric) },
                       { "MEDIAN", new IntrinsicFunction("MEDIAN", FunctionType.Numeric) },
                       { "MIDRANGE", new IntrinsicFunction("MIDRANGE", FunctionType.Numeric) },
                       { "MIN", new IntrinsicFunction("MIN", FunctionType.Numeric) }, //Depends on arguments
                       { "MOD", new IntrinsicFunction("MOD", FunctionType.Integer) },
                       { "NATIONAL-OF", new IntrinsicFunction("NATIONAL-OF", FunctionType.National) },
                       { "NUMVAL", new IntrinsicFunction("NUMVAL", FunctionType.Numeric) },
                       { "NUMVAL-C", new IntrinsicFunction("NUMVAL-C", FunctionType.Numeric) },
                       { "ORD", new IntrinsicFunction("ORD", FunctionType.Integer) },
                       { "ORD-MAX", new IntrinsicFunction("ORD-MAX", FunctionType.Integer) },
                       { "ORD-MIN", new IntrinsicFunction("ORD-MIN", FunctionType.Integer) },
                       { "PRESENT-VALUE", new IntrinsicFunction("PRESENT-VALUE", FunctionType.Numeric) },
                       { "RANDOM", new IntrinsicFunction("RANDOM", FunctionType.Numeric) },
                       { "RANGE", new IntrinsicFunction("RANGE", FunctionType.Numeric) }, //Could be Integer depending on the arguments
                       { "REM", new IntrinsicFunction("REM", FunctionType.Numeric) },
                       { "REVERSE", new IntrinsicFunction("REVERSE", FunctionType.Alphanumeric) }, //Could be National depending on argument type
                       { "SIN", new IntrinsicFunction("SIN", FunctionType.Numeric) },
                       { "SQRT", new IntrinsicFunction("SQRT", FunctionType.Numeric) },
                       { "STANDARD-DEVIATION", new IntrinsicFunction("STANDARD-DEVIATION", FunctionType.Numeric) },
                       { "SUM", new IntrinsicFunction("SUM", FunctionType.Numeric) }, //Could be Integer depending on the arguments
                       { "TAN", new IntrinsicFunction("TAN", FunctionType.Numeric) },
                       { "ULENGTH", new IntrinsicFunction("ULENGTH", FunctionType.Integer) },
                       { "UPOS", new IntrinsicFunction("UPOS", FunctionType.Integer) },
                       { "UPPER-CASE", new IntrinsicFunction("UPPER-CASE", FunctionType.Alphanumeric) }, //Could be National depending on argument type
                       { "USUBSTR", new IntrinsicFunction("USUBSTR", FunctionType.Alphanumeric) }, //Could be National depending on argument type
                       { "USUPPLEMENTARY", new IntrinsicFunction("USUPPLEMENTARY", FunctionType.Integer) },
                       { "UVALID", new IntrinsicFunction("UVALID", FunctionType.Integer) },
                       { "UWIDTH", new IntrinsicFunction("UWIDTH", FunctionType.Integer) },
                       { "VARIANCE", new IntrinsicFunction("VARIANCE", FunctionType.Numeric) },
                       { "WHEN-COMPILED", new IntrinsicFunction("WHEN-COMPILED", FunctionType.Alphanumeric) },
                       { "YEAR-TO-YYYY", new IntrinsicFunction("YEAR-TO-YYYY", FunctionType.Integer) }
                   };
        }

        public static bool TryGet(string name, out IntrinsicFunction intrinsicFunction) => _All.TryGetValue(name, out intrinsicFunction);

        public static IntrinsicFunction Get(string name) => _All[name];

        private IntrinsicFunction(string name, FunctionType functionType)
            : base(new GeneratedSymbolName(null, name), SymbolType.IntrinsicFunctionName)
        {
            FunctionType = functionType;
        }

        public FunctionType FunctionType { get; }
    }
}

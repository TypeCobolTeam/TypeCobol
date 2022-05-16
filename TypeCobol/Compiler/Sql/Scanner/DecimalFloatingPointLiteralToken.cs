using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.Scanner
{
    public enum DecimalFloatingPointSpecialValueType
    {
        NaN,
        SNaN,
        Infinity
    }

    public readonly struct DecimalFloatingPointSpecialValue
    {
        public DecimalFloatingPointSpecialValue(bool isNegative, DecimalFloatingPointSpecialValueType decimalFloatingPointSpecialValue)
        {
            IsNegative = isNegative;
            Type = decimalFloatingPointSpecialValue;
        }

        public bool IsNegative { get; }

        public DecimalFloatingPointSpecialValueType Type { get; }
    }

    public class DecimalFloatingPointLiteralTokenValue : LiteralTokenValue
    {
        public DecimalFloatingPointSpecialValue? SpecialValue { get; set; }

        public FloatingPointLiteralTokenValue NumericConstant { get; }


        public DecimalFloatingPointLiteralTokenValue(LiteralTokenValueType type, FloatingPointLiteralTokenValue numericConstant) : base(type)
        {
            NumericConstant = numericConstant;
        }
    }

}

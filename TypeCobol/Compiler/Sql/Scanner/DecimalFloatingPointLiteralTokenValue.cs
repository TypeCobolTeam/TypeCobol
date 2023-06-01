#nullable enable

using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.Scanner
{
    public enum DecimalFloatingPointSpecialValueType
    {
        NaN,
        SNaN,
        Infinity
    }

    public class DecimalFloatingPointSpecialValue
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
        public DecimalFloatingPointSpecialValue? SpecialValue { get; }

        public FloatingPointLiteralTokenValue? NumericConstant { get; }

        public DecimalFloatingPointLiteralTokenValue(FloatingPointLiteralTokenValue numericConstant) : base(LiteralTokenValueType.DecimalFloatingPoint)
        {
            NumericConstant = numericConstant;
        }

        public DecimalFloatingPointLiteralTokenValue(DecimalFloatingPointSpecialValue decimalFloatingPointSpecialValue) : base(LiteralTokenValueType.DecimalFloatingPoint)
        {
            SpecialValue = decimalFloatingPointSpecialValue;
        }
    }
}

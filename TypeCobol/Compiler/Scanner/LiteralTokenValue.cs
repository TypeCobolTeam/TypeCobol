using System;
using System.Globalization;
using System.Numerics;
using System.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Base class for all types of literal token values
    /// </summary>
    public abstract class LiteralTokenValue
    {
        protected LiteralTokenValue(LiteralTokenValueType type)
        {
            Type = type;
        }

        public LiteralTokenValueType Type { get; }
    }

    public enum LiteralTokenValueType
    {
        Alphanumeric,
        Integer,
        Decimal,
        FloatingPoint,
        DecimalFloatingPoint
    }

    /// <summary>
    /// Value for all alphanumeric literal tokens
    /// </summary>
    public class AlphanumericLiteralTokenValue : LiteralTokenValue
    {
        public AlphanumericLiteralTokenValue(string text) : base(LiteralTokenValueType.Alphanumeric)
        {
            Text = text;
        }

        public AlphanumericLiteralTokenValue(string hexadecimalChars, Encoding encoding) : base(LiteralTokenValueType.Alphanumeric)
        {
            Text = encoding.GetString(StringToByteArray(hexadecimalChars));
        }

        private static byte[] StringToByteArray(string hexadecimalChars)
        {
            byte[] arr = new byte[hexadecimalChars.Length >> 1];

            for (int i = 0; i < hexadecimalChars.Length >> 1; ++i)
            {
                arr[i] = (byte)((GetHexVal(hexadecimalChars[i << 1]) << 4) + (GetHexVal(hexadecimalChars[(i << 1) + 1])));
            }

            return arr;
        }

        private static int GetHexVal(char hex)
        {
            // For uppercase A-F letters and for lowercase a-f letters
            return hex - (hex < 58 ? 48 : (hex < 97 ? 55 : 87));
        }

        public string Text { get; }

        /// <summary>
        /// For test and debugging purposes
        /// </summary>
        public override string ToString()
        {
            return Text;
        }
    }
    
    /// <summary>
    /// Value for token type IntegerLiteral
    /// </summary>
    public class IntegerLiteralTokenValue : LiteralTokenValue
    {
        public IntegerLiteralTokenValue(string sign, string number) : base(LiteralTokenValueType.Integer)
        {
            HasSign = !string.IsNullOrEmpty(sign);
            Number = BigInteger.Parse(number);

            if (sign == "-")
            {
                Number = -Number;
            }
        }

        /// <summary>
        /// True if a sign was explicitly written as the first character of the integer literal
        /// </summary>
        public bool HasSign { get; }

        public BigInteger Number { get; }

        /// <summary>
        /// For test and debugging purposes
        /// </summary>
        public override string ToString()
        {
            return Number.ToString(NumberFormatInfo.InvariantInfo);
        }
    }

    /// <summary>
    /// Value for token type DecimalLiteral
    /// </summary>
    public class DecimalLiteralTokenValue : LiteralTokenValue
    {
        public DecimalLiteralTokenValue(string sign, string integerPart, string decimalPart) : base(LiteralTokenValueType.Decimal)
        {
            IntegerValue = BigInteger.Parse(integerPart + decimalPart);
            DecimalDigits = decimalPart.Length;
            Number = (double)IntegerValue / Math.Pow(10, DecimalDigits);
            if (sign == "-")
            {
                IntegerValue = -IntegerValue;
                Number = -Number;
            }
        }

        /// <summary>
        /// For the number 5.43, the integer value is 543
        /// </summary>
        public BigInteger IntegerValue { get; }

        /// <summary>
        /// For the number 5.43, the number of decimal digits is 2
        /// </summary>
        public int  DecimalDigits { get; }

        public double Number { get; }

        /// <summary>
        /// For test and debugging purposes
        /// </summary>
        public override string ToString()
        {
            return IntegerValue + "|" + DecimalDigits + ">" + Number.ToString(NumberFormatInfo.InvariantInfo);
        }
    }

    /// <summary>
    /// Value for token type DecimalLiteral
    /// </summary>
    public class FloatingPointLiteralTokenValue : LiteralTokenValue
    {
        public FloatingPointLiteralTokenValue(string mantissaSign, string mantissaIntegerPart, string mantissaDecimalPart, string exponentSign, string exponentNumber) : base(LiteralTokenValueType.FloatingPoint)
        {
            Mantissa = new DecimalLiteralTokenValue(mantissaSign, mantissaIntegerPart, mantissaDecimalPart);
            Exponent = new IntegerLiteralTokenValue(exponentSign, exponentNumber);

            Number = Mantissa.Number * Math.Pow(10, (double)Exponent.Number);
        }

        public DecimalLiteralTokenValue Mantissa { get; }

        public IntegerLiteralTokenValue Exponent { get; }

        public double Number { get; }

        /// <summary>
        /// For test and debugging purposes
        /// </summary>
        public override string ToString()
        {
            return Mantissa.IntegerValue + "|" +  Mantissa.DecimalDigits + ">" + Mantissa.Number.ToString(NumberFormatInfo.InvariantInfo) + "E" + Exponent.Number + ">" + Number.ToString(NumberFormatInfo.InvariantInfo);
        }
    }
}

using System;
using System.Globalization;
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

        public LiteralTokenValueType Type { get; private set; }
    }

    public enum LiteralTokenValueType
    {
        Alphanumeric,
        FigurativeConstant,
        SymbolicCharacter,
        Integer,
        Decimal,
        FloatingPoint
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
            int val = (int)hex;
            // For uppercase A-F letters and for lowercase a-f letters
            return val - (val < 58 ? 48 : (val < 97 ? 55 : 87));
        }

        // Property set later at the parsing stage
        public bool All { get; set; }

        public string Text { get; private set; }

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
            HasSign = !String.IsNullOrEmpty(sign);
            Number = Int64.Parse(number);



            if (sign == "-")
            {
                Number = -Number;
            }
        }

        /// <summary>
        /// Long number instead of string
        /// </summary>
        public IntegerLiteralTokenValue(string sign, long number) : base(LiteralTokenValueType.Integer)
        {
            HasSign = !String.IsNullOrEmpty(sign);
            Number = number;



            if (sign == "-")
            {
                Number = -Number;
            }
        }

        /// <summary>
        /// True if a sign was explicitely written as the first character of the integer literal
        /// </summary>
        public bool HasSign { get; private set; }

        public long Number { get; private set; }

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
            IntegerValue = Int64.Parse(integerPart + decimalPart);
            DecimalDigits = decimalPart.Length;
            Number = IntegerValue / Math.Pow(10, DecimalDigits);
            if (sign == "-")
            {
                IntegerValue = -IntegerValue;
                Number = -Number;
            }
        }

        public DecimalLiteralTokenValue(double value) : base(LiteralTokenValueType.Decimal)
        {
            Number = value;
        }

        /// <summary>
        /// For the number 5.43, the integer value is 543
        /// </summary>
        public long  IntegerValue  { get; private set; }
        /// <summary>
        /// For the number 5.43, the number of decimal digits is 2
        /// </summary>
        public int  DecimalDigits { get; private set; }

        public double Number { get; private set; }

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

            Number = Mantissa.Number * Math.Pow(10, Exponent.Number);
        }

        public DecimalLiteralTokenValue Mantissa { get; private set; }
        public IntegerLiteralTokenValue Exponent { get; private set; }

        public double Number { get; private set; }

        /// <summary>
        /// For test and debugging purposes
        /// </summary>
        public override string ToString()
        {
            return Mantissa.IntegerValue + "|" +  Mantissa.DecimalDigits + ">" + Mantissa.Number.ToString(NumberFormatInfo.InvariantInfo) + "E" + Exponent.Number + ">" + Number.ToString(NumberFormatInfo.InvariantInfo);
        }
    }
}

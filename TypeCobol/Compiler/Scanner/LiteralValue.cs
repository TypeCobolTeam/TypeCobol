using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Base class for all types of literal values
    /// </summary>
    public abstract class LiteralValue
    {
        public LiteralValue(LiteralValueType type)
        {
            Type = type;
        }

        public LiteralValueType Type { get; private set; }
    }

    public enum LiteralValueType
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
    public class AlphanumericLiteralValue : LiteralValue
    {
        public AlphanumericLiteralValue(string text) : base(LiteralValueType.Alphanumeric)
        {
            Text = text;
        }

        public AlphanumericLiteralValue(string hexadecimalChars, Encoding encoding) : base(LiteralValueType.Alphanumeric)
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
    public class IntegerLiteralValue : LiteralValue
    {
        public IntegerLiteralValue(string sign, string number) : base(LiteralValueType.Integer)
        {
            if(!String.IsNullOrEmpty(sign))
            {
                HasSign = true;
            }
            else
            {
                HasSign = false;
            }
            Number = Int64.Parse(number);
            if(sign == "-")
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
            return Number.ToString();
        }
    }

    /// <summary>
    /// Value for token type DecimalLiteral
    /// </summary>
    public class DecimalLiteralValue : LiteralValue
    {
        public DecimalLiteralValue(string sign, string integerPart, string decimalPart) : base(LiteralValueType.Decimal)
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
            return IntegerValue + "|" + DecimalDigits + ">" + Number;
        }
    }

    /// <summary>
    /// Value for token type DecimalLiteral
    /// </summary>
    public class FloatingPointLiteralValue : LiteralValue
    {
        public FloatingPointLiteralValue(string mantissaSign, string mantissaIntegerPart, string mantissaDecimalPart, string exponentSign, string exponentNumber) : base(LiteralValueType.FloatingPoint)
        {
            Mantissa = new DecimalLiteralValue(mantissaSign, mantissaIntegerPart, mantissaDecimalPart);
            Exponent = new IntegerLiteralValue(exponentSign, exponentNumber);

            Number = Mantissa.Number * Math.Pow(10, Exponent.Number);
        }

        public DecimalLiteralValue Mantissa { get; private set; }
        public IntegerLiteralValue Exponent { get; private set; }

        public double Number { get; private set; }

        /// <summary>
        /// For test and debugging purposes
        /// </summary>
        public override string ToString()
        {
            return Mantissa.IntegerValue + "|" +  Mantissa.DecimalDigits + ">" + Mantissa.Number + "E" + Exponent.Number + ">" + Number;
        }
    }
}

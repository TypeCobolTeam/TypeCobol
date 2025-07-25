using System;

namespace TypeCobol.Compiler.File
{
    // --- NB : The classes below are not used in the current version, but may be useful in the future to support Cobol files with DBCS characters, or IBM code pages with no equivalent .Net Encoding ---

    /// <summary>
    /// Character types defined by the Cobol Language
    /// </summary>
    internal enum CharacterType
    {
        // Common character type
        BasicCobolChar,
        // EBCDIC character types
        SingleByteChar,
        DBCSShiftOut,
        DBCSShiftIn,
        DBCSChar,
        // Non EBCDIC character type
        UnicodeChar,
        // Special character types
        NewLine,
        EndOfFile
    }

    /// <summary>
    /// Character names and character types for common EBCDIC character codes
    /// </summary>
    internal class EBCDICCharacterCodes
    {
        // Basic COBOL character set

        // Special characters 
        public static readonly byte Space = 64;
        public static readonly byte PlusSign = 78;
        public static readonly byte MinusSignOrHyphen = 96;
        public static readonly byte Asterisk = 92;
        public static readonly byte ForwardSlashOrSolidus = 97;
        public static readonly byte EqualSign = 126;
        public static readonly byte CurrencySign = 91; // The currency sign is the character with the value X'5B', regardless of the code page in effect. The assigned graphic character can be the dollar sign or a local currency sign.
        public static readonly byte Comma = 107;
        public static readonly byte Semicolon = 94;
        public static readonly byte DecimalPointOrPeriod = 75;
        public static readonly byte QuotationMark = 127; // The quotation mark is the character with the value X'7F'
        public static readonly byte Apostrophe = 125;
        public static readonly byte LeftParenthesis = 77;
        public static readonly byte RightParenthesis = 93;
        public static readonly byte GreaterThan = 110;
        public static readonly byte LessThan = 76;
        public static readonly byte Colon = 122;
        public static readonly byte Underscore = 109;

        // A - Z Alphabet (uppercase)
        public static readonly byte A = 193;
        public static readonly byte B = 194;
        public static readonly byte C = 195;
        public static readonly byte D = 196;
        public static readonly byte E = 197;
        public static readonly byte F = 198;
        public static readonly byte G = 199;
        public static readonly byte H = 200;
        public static readonly byte I = 201;
        public static readonly byte J = 209;
        public static readonly byte K = 210;
        public static readonly byte L = 211;
        public static readonly byte M = 212;
        public static readonly byte N = 213;
        public static readonly byte O = 214;
        public static readonly byte P = 215;
        public static readonly byte Q = 216;
        public static readonly byte R = 217;
        public static readonly byte S = 226;
        public static readonly byte T = 227;
        public static readonly byte U = 228;
        public static readonly byte V = 229;
        public static readonly byte W = 230;
        public static readonly byte X = 231;
        public static readonly byte Y = 232;
        public static readonly byte Z = 233;

        // a - z Alphabet (lowercase)
        public static readonly byte a = 129;
        public static readonly byte b = 130;
        public static readonly byte c = 131;
        public static readonly byte d = 132;
        public static readonly byte e = 133;
        public static readonly byte f = 134;
        public static readonly byte g = 135;
        public static readonly byte h = 136;
        public static readonly byte i = 137;
        public static readonly byte j = 145;
        public static readonly byte k = 146;
        public static readonly byte l = 147;
        public static readonly byte m = 148;
        public static readonly byte n = 149;
        public static readonly byte o = 150;
        public static readonly byte p = 151;
        public static readonly byte q = 152;
        public static readonly byte r = 153;
        public static readonly byte s = 162;
        public static readonly byte t = 163;
        public static readonly byte u = 164;
        public static readonly byte v = 165;
        public static readonly byte w = 166;
        public static readonly byte x = 167;
        public static readonly byte y = 168;
        public static readonly byte z = 169;

        // 0 - 9 Numeric characters
        public static readonly byte _0 = 240;
        public static readonly byte _1 = 241;
        public static readonly byte _2 = 242;
        public static readonly byte _3 = 243;
        public static readonly byte _4 = 244;
        public static readonly byte _5 = 245;
        public static readonly byte _6 = 246;
        public static readonly byte _7 = 247;
        public static readonly byte _8 = 248;
        public static readonly byte _9 = 249;

        // ---
        // New lines

        public static readonly byte CR = 13;
        public static readonly byte NL = 21;
        public static readonly byte LF = 37;

        // DBCS ShiftOut / ShiftIn
        public static readonly byte DBCSShiftOut = 14;
        public static readonly byte DBCSShiftIn = 15;

        //---
        // Character types
        public static readonly CharacterType[] CharacterType = new CharacterType[256];

        static EBCDICCharacterCodes()
        {
            // Default type
            for(int i = 0 ; i<256 ; i++)
            {
                CharacterType[i] = File.CharacterType.SingleByteChar;
            }

            // Basic COBOL character set 
            CharacterType[Space] = File.CharacterType.BasicCobolChar;
            CharacterType[PlusSign] = File.CharacterType.BasicCobolChar;
            CharacterType[MinusSignOrHyphen] = File.CharacterType.BasicCobolChar;
            CharacterType[Asterisk] = File.CharacterType.BasicCobolChar;
            CharacterType[ForwardSlashOrSolidus] = File.CharacterType.BasicCobolChar;
            CharacterType[EqualSign] = File.CharacterType.BasicCobolChar;
            CharacterType[CurrencySign] = File.CharacterType.BasicCobolChar;
            CharacterType[Comma] = File.CharacterType.BasicCobolChar;
            CharacterType[Semicolon] = File.CharacterType.BasicCobolChar;
            CharacterType[DecimalPointOrPeriod] = File.CharacterType.BasicCobolChar;
            CharacterType[QuotationMark] = File.CharacterType.BasicCobolChar;
            CharacterType[Apostrophe] = File.CharacterType.BasicCobolChar;
            CharacterType[LeftParenthesis] = File.CharacterType.BasicCobolChar;
            CharacterType[RightParenthesis] = File.CharacterType.BasicCobolChar;
            CharacterType[GreaterThan] = File.CharacterType.BasicCobolChar;
            CharacterType[LessThan] = File.CharacterType.BasicCobolChar;
            CharacterType[Colon] = File.CharacterType.BasicCobolChar;
            CharacterType[Underscore] = File.CharacterType.BasicCobolChar;
            // A - Z Alphabet (uppercase)
            for(int i = 193 ; i<=201 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            for(int i = 209 ; i<=217 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            for(int i = 226 ; i<=233 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            // a - z Alphabet (lowercase)
            for(int i = 129 ; i<=137 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            for(int i = 145 ; i<=153 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            for(int i = 162 ; i<=169 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            // 0 - 9 Numeric characters
            for (int i = 240; i <= 249; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }

            // New lines
            CharacterType[CR] = File.CharacterType.NewLine;
            CharacterType[NL] = File.CharacterType.NewLine;
            CharacterType[LF] = File.CharacterType.NewLine;

            // DBCS ShiftOut / ShiftIn
            CharacterType[DBCSShiftOut] = File.CharacterType.DBCSShiftOut;
            CharacterType[DBCSShiftIn] = File.CharacterType.DBCSShiftIn;
        }
    }

    /// <summary>
    /// Character names and character types for common ASCII character codes
    /// </summary>
    internal class ASCIICharacterCodes
    {
        // Basic COBOL character set

        // Special characters 
        public static readonly byte Space = 32;
        public static readonly byte PlusSign = 43;
        public static readonly byte MinusSignOrHyphen = 45;
        public static readonly byte Asterisk = 42;
        public static readonly byte ForwardSlashOrSolidus = 47;
        public static readonly byte EqualSign = 61;
        public static readonly byte CurrencySign = 36;
        public static readonly byte Comma = 44;
        public static readonly byte Semicolon = 59;
        public static readonly byte DecimalPointOrPeriod = 46;
        public static readonly byte QuotationMark = 34;
        public static readonly byte Apostrophe = 39;
        public static readonly byte LeftParenthesis = 40;
        public static readonly byte RightParenthesis = 41;
        public static readonly byte GreaterThan = 62;
        public static readonly byte LessThan = 60;
        public static readonly byte Colon = 58;
        public static readonly byte Underscore = 95;

        // A - Z Alphabet (uppercase)
        public static readonly byte A = 65;
        public static readonly byte B = 66;
        public static readonly byte C = 67;
        public static readonly byte D = 68;
        public static readonly byte E = 69;
        public static readonly byte F = 70;
        public static readonly byte G = 71;
        public static readonly byte H = 72;
        public static readonly byte I = 73;
        public static readonly byte J = 74;
        public static readonly byte K = 75;
        public static readonly byte L = 76;
        public static readonly byte M = 77;
        public static readonly byte N = 78;
        public static readonly byte O = 79;
        public static readonly byte P = 80;
        public static readonly byte Q = 81;
        public static readonly byte R = 82;
        public static readonly byte S = 83;
        public static readonly byte T = 84;
        public static readonly byte U = 85;
        public static readonly byte V = 86;
        public static readonly byte W = 87;
        public static readonly byte X = 88;
        public static readonly byte Y = 89;
        public static readonly byte Z = 90;

        // a - z Alphabet (lowercase)
        public static readonly byte a = 97;
        public static readonly byte b = 98;
        public static readonly byte c = 99;
        public static readonly byte d = 100;
        public static readonly byte e = 101;
        public static readonly byte f = 102;
        public static readonly byte g = 103;
        public static readonly byte h = 104;
        public static readonly byte i = 105;
        public static readonly byte j = 106;
        public static readonly byte k = 107;
        public static readonly byte l = 108;
        public static readonly byte m = 109;
        public static readonly byte n = 110;
        public static readonly byte o = 111;
        public static readonly byte p = 112;
        public static readonly byte q = 113;
        public static readonly byte r = 114;
        public static readonly byte s = 115;
        public static readonly byte t = 116;
        public static readonly byte u = 117;
        public static readonly byte v = 118;
        public static readonly byte w = 119;
        public static readonly byte x = 120;
        public static readonly byte y = 121;
        public static readonly byte z = 122;
        
        // ---
        // New lines

        public static readonly byte CR = 13;
        public static readonly byte LF = 10;

        //---
        // Character types
        public static readonly CharacterType[] CharacterType = new CharacterType[256];

        static ASCIICharacterCodes()
        {
            // Default type
            for(int i = 0 ; i<256 ; i++)
            {
                CharacterType[i] = File.CharacterType.UnicodeChar;
            }

            // Basic COBOL character set 
            CharacterType[Space] = File.CharacterType.BasicCobolChar;
            CharacterType[PlusSign] = File.CharacterType.BasicCobolChar;
            CharacterType[MinusSignOrHyphen] = File.CharacterType.BasicCobolChar;
            CharacterType[Asterisk] = File.CharacterType.BasicCobolChar;
            CharacterType[ForwardSlashOrSolidus] = File.CharacterType.BasicCobolChar;
            CharacterType[EqualSign] = File.CharacterType.BasicCobolChar;
            CharacterType[CurrencySign] = File.CharacterType.BasicCobolChar;
            CharacterType[Comma] = File.CharacterType.BasicCobolChar;
            CharacterType[Semicolon] = File.CharacterType.BasicCobolChar;
            CharacterType[DecimalPointOrPeriod] = File.CharacterType.BasicCobolChar;
            CharacterType[QuotationMark] = File.CharacterType.BasicCobolChar;
            CharacterType[Apostrophe] = File.CharacterType.BasicCobolChar;
            CharacterType[LeftParenthesis] = File.CharacterType.BasicCobolChar;
            CharacterType[RightParenthesis] = File.CharacterType.BasicCobolChar;
            CharacterType[GreaterThan] = File.CharacterType.BasicCobolChar;
            CharacterType[LessThan] = File.CharacterType.BasicCobolChar;
            CharacterType[Colon] = File.CharacterType.BasicCobolChar;
            CharacterType[Underscore] = File.CharacterType.BasicCobolChar;
            // A - Z Alphabet (uppercase)
            for(int i = 65 ; i<= 90 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            // a - z Alphabet (lowercase)
            for(int i = 97 ; i<= 122 ; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }
            // 0 - 9 Numeric characters
            for (int i = 48; i <= 57; i++)
            {
                CharacterType[i] = File.CharacterType.BasicCobolChar;
            }

            // New lines
            CharacterType[CR] = File.CharacterType.NewLine;
            CharacterType[LF] = File.CharacterType.NewLine;
        }
    }
}

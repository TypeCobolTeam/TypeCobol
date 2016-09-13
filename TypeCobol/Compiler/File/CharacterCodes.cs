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
        public static byte Space = 64;
        public static byte PlusSign = 78;
        public static byte MinusSignOrHyphen = 96;
        public static byte Asterisk = 92;
        public static byte ForwardSlashOrSolidus = 97;
        public static byte EqualSign = 126;
        public static byte CurrencySign = 91; // The currency sign is the character with the value X'5B', regardless of the code page in effect. The assigned graphic character can be the dollar sign or a local currency sign.
        public static byte Comma = 107;
        public static byte Semicolon = 94;
        public static byte DecimalPointOrPeriod = 75;
        public static byte QuotationMark = 127; // The quotation mark is the character with the value X'7F'
        public static byte Apostrophe = 125;
        public static byte LeftParenthesis = 77;
        public static byte RightParenthesis = 93;
        public static byte GreaterThan = 110;
        public static byte LessThan = 76;
        public static byte Colon = 122;
        public static byte Underscore = 109;

        // A - Z Alphabet (uppercase)
        public static byte A = 193;
        public static byte B = 194;
        public static byte C = 195;
        public static byte D = 196;
        public static byte E = 197;
        public static byte F = 198;
        public static byte G = 199;
        public static byte H = 200;
        public static byte I = 201;
        public static byte J = 209;
        public static byte K = 210;
        public static byte L = 211;
        public static byte M = 212;
        public static byte N = 213;
        public static byte O = 214;
        public static byte P = 215;
        public static byte Q = 216;
        public static byte R = 217;
        public static byte S = 226;
        public static byte T = 227;
        public static byte U = 228;
        public static byte V = 229;
        public static byte W = 230;
        public static byte X = 231;
        public static byte Y = 232;
        public static byte Z = 233;

        // a - z Alphabet (lowercase)
        public static byte a = 129;
        public static byte b = 130;
        public static byte c = 131;
        public static byte d = 132;
        public static byte e = 133;
        public static byte f = 134;
        public static byte g = 135;
        public static byte h = 136;
        public static byte i = 137;
        public static byte j = 145;
        public static byte k = 146;
        public static byte l = 147;
        public static byte m = 148;
        public static byte n = 149;
        public static byte o = 150;
        public static byte p = 151;
        public static byte q = 152;
        public static byte r = 153;
        public static byte s = 162;
        public static byte t = 163;
        public static byte u = 164;
        public static byte v = 165;
        public static byte w = 166;
        public static byte x = 167;
        public static byte y = 168;
        public static byte z = 169;

        // 0 - 9 Numeric characters
        public static byte _0 = 240;
        public static byte _1 = 241;
        public static byte _2 = 242;
        public static byte _3 = 243;
        public static byte _4 = 244;
        public static byte _5 = 245;
        public static byte _6 = 246;
        public static byte _7 = 247;
        public static byte _8 = 248;
        public static byte _9 = 249;

        // ---
        // New lines

        public static byte CR = 13;
        public static byte NL = 21;
        public static byte LF = 37;

        // DBCS ShiftOut / ShiftIn
        public static byte DBCSShiftOut = 14;
        public static byte DBCSShiftIn = 15;

        //---
        // Character types
        public static CharacterType[] CharacterType = new CharacterType[256];

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
        public static byte Space = 32;
        public static byte PlusSign = 43;
        public static byte MinusSignOrHyphen = 45;
        public static byte Asterisk = 42;
        public static byte ForwardSlashOrSolidus = 47;
        public static byte EqualSign = 61;
        public static byte CurrencySign = 36;
        public static byte Comma = 44;
        public static byte Semicolon = 59;
        public static byte DecimalPointOrPeriod = 46;
        public static byte QuotationMark = 34;
        public static byte Apostrophe = 39;
        public static byte LeftParenthesis = 40;
        public static byte RightParenthesis = 41;
        public static byte GreaterThan = 62;
        public static byte LessThan = 60;
        public static byte Colon = 58;
        public static byte Underscore = 95;

        // A - Z Alphabet (uppercase)
        public static byte A = 65;
        public static byte B = 66;
        public static byte C = 67;
        public static byte D = 68;
        public static byte E = 69;
        public static byte F = 70;
        public static byte G = 71;
        public static byte H = 72;
        public static byte I = 73;
        public static byte J = 74;
        public static byte K = 75;
        public static byte L = 76;
        public static byte M = 77;
        public static byte N = 78;
        public static byte O = 79;
        public static byte P = 80;
        public static byte Q = 81;
        public static byte R = 82;
        public static byte S = 83;
        public static byte T = 84;
        public static byte U = 85;
        public static byte V = 86;
        public static byte W = 87;
        public static byte X = 88;
        public static byte Y = 89;
        public static byte Z = 90;

        // a - z Alphabet (lowercase)
        public static byte a = 97;
        public static byte b = 98;
        public static byte c = 99;
        public static byte d = 100;
        public static byte e = 101;
        public static byte f = 102;
        public static byte g = 103;
        public static byte h = 104;
        public static byte i = 105;
        public static byte j = 106;
        public static byte k = 107;
        public static byte l = 108;
        public static byte m = 109;
        public static byte n = 110;
        public static byte o = 111;
        public static byte p = 112;
        public static byte q = 113;
        public static byte r = 114;
        public static byte s = 115;
        public static byte t = 116;
        public static byte u = 117;
        public static byte v = 118;
        public static byte w = 119;
        public static byte x = 120;
        public static byte y = 121;
        public static byte z = 122;
        
        // ---
        // New lines

        public static byte CR = 13;
        public static byte LF = 10;

        //---
        // Character types
        public static CharacterType[] CharacterType = new CharacterType[256];

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

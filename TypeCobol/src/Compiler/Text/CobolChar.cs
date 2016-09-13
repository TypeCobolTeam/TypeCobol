﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Check the possible role of Cobol characters in source text
    /// </summary>
    public static class CobolChar
    {
        private static bool[] isCobolWordChar = new bool[256];
        private static bool[] isCobolWordSeparator = new bool[256];
        private static bool[] isStartOfPictureCharacterString = new bool[256];

        /// <summary>
        /// True if the current char can be found inside a Cobol word
        /// </summary>
        public static bool IsCobolWordChar(char chr)
        {
            return isCobolWordChar[(byte)chr];
        }

        /// <summary>
        /// True if the current char can be a separator between two Cobol words
        /// </summary>
        public static bool IsCobolWordSeparator(char chr)
        {
            return isCobolWordSeparator[(byte)chr];
        }

        /// <summary>
        /// True if the current char can start a PICTURE character string
        /// </summary>
        public static bool IsStartOfPictureCharacterString(char chr)
        {
            return isStartOfPictureCharacterString[(byte)chr];
        }

        static CobolChar()
        {
            // --- isCobolWordChar ---

            for (int i = 0; i < 256; i++)
            {
                isCobolWordChar[i] = false;
            }
            // list of all chars allowed in a Cobol word
            // Latin uppercase letters A through Z
            for (int i = (byte)'A'; i <= (byte)'Z'; i++)
            {
                isCobolWordChar[i] = true;
            }
            // Latin lowercase letters a through z
            for (int i = (byte)'a'; i <= (byte)'z'; i++)
            {
                isCobolWordChar[i] = true;
            }
            // digits 0 through 9
            for (int i = (byte)'0'; i <= (byte)'9'; i++)
            {
                isCobolWordChar[i] = true;
            }
            // - (hyphen)
            isCobolWordChar[(byte)'-'] = true;
            // _ (underscore)
            isCobolWordChar[(byte)'_'] = true;

            // --- isCobolWordSeparator ---

            for(int i = 0 ; i < 256 ; i++)
            {
                isCobolWordSeparator[i] = false;
            }
            // list of all chars which begin a different token type 
            // and are not allowed in a reserved word or user defined word
            isCobolWordSeparator[(byte)' '] = true;
            isCobolWordSeparator[(byte)','] = true;
            isCobolWordSeparator[(byte)';'] = true;
            isCobolWordSeparator[(byte)'*'] = true;
            isCobolWordSeparator[(byte)'.'] = true;
            isCobolWordSeparator[(byte)':'] = true;
            isCobolWordSeparator[(byte)'('] = true;
            isCobolWordSeparator[(byte)')'] = true;
            isCobolWordSeparator[(byte)'+'] = true;
            // allowed : isCobolWordSeparator[(byte)'-'] = true;
            isCobolWordSeparator[(byte)'/'] = true;
            isCobolWordSeparator[(byte)'<'] = true;
            isCobolWordSeparator[(byte)'>'] = true;
            isCobolWordSeparator[(byte)'='] = true;
            isCobolWordSeparator[(byte)'"'] = true;
            isCobolWordSeparator[(byte)'\''] = true;

            // --- isStartOfPictureCharacterString ---

            // DECIMAL-POINT IS COMMA exchanges the functions of the period and the comma in PICTURE character-strings
            // The currency symbol in a picture character-string is represented by the default currency symbol $ or by a single character specified either in the CURRENCY compiler option or in the CURRENCY SIGN clause in the SPECIAL-NAMES paragraph of the ENVIRONMENT DIVISION.

            // Characters of a picture character string:
            // A, B, C, D, E, G, N, P, R, S, V, X, Z
            // a, b, c, d, e, g, n, p, r, s, v, x, z
            // 0, 9
            // / , . + - *
            // $ [currencysymbol]

            // Although the default currency symbol is represented by $ in this document, the actual default currency symbol is the character with the value X'5B' in the EBCDIC code page in effect at compile time.
            // [currency symbol] can be anything except
            // - Digits 0 through 9 
            // - Alphabetic characters A, B, C, D, E, G, N, P, R, S, V, X, Z, their lowercase equivalents
            // - The space 
            // - Special characters + - , . * / ; ( )" = ’

            // So we can only be sure that the following characters can NOT start a picture character string
            // 1-8, space, ; ( )" = ’

            for (int i = 0; i < 256; i++)
            {
                isStartOfPictureCharacterString[i] = true;
            }
            isStartOfPictureCharacterString[(byte)'1'] = false;
            isStartOfPictureCharacterString[(byte)'2'] = false;
            isStartOfPictureCharacterString[(byte)'3'] = false;
            isStartOfPictureCharacterString[(byte)'4'] = false;
            isStartOfPictureCharacterString[(byte)'5'] = false;
            isStartOfPictureCharacterString[(byte)'6'] = false;
            isStartOfPictureCharacterString[(byte)'7'] = false;
            isStartOfPictureCharacterString[(byte)'8'] = false;
            isStartOfPictureCharacterString[(byte)' '] = false;
            isStartOfPictureCharacterString[(byte)';'] = false;
            isStartOfPictureCharacterString[(byte)'('] = false;
            isStartOfPictureCharacterString[(byte)')'] = false;
            isStartOfPictureCharacterString[(byte)'"'] = false;
            isStartOfPictureCharacterString[(byte)'='] = false;
            isStartOfPictureCharacterString[(byte)'\''] = false;
        }
    }
}

using System;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// Special characters of a picture character string.
        /// </summary>
        internal enum SC
        {
            B,
            ZERO, // 0
            SLASH, // /
            COMMA, // ,
            DOT, // .
            PLUS, // +
            MINUS, // -
            CR,
            DB,
            CS,
            E,
            Z,
            STAR, // *
            NINE, // 9                        
            A,
            X,
            S,
            V,
            P,
            G,
            N
        }

        //Does not handle CR, DB and CS
        private static SC Char2SC(char c)
        {
            switch (char.ToUpper(c))
            {
                case 'A':
                    return SC.A;
                case 'B':
                    return SC.B;
                case 'E':
                    return SC.E;
                case 'G':
                    return SC.G;
                case 'N':
                    return SC.N;
                case 'P':
                    return SC.P;
                case 'S':
                    return SC.S;
                case 'V':
                    return SC.V;
                case 'X':
                    return SC.X;
                case 'Z':
                    return SC.Z;
                case '9':
                    return SC.NINE;
                case '0':
                    return SC.ZERO;
                case '/':
                    return SC.SLASH;
                case ',':
                    return SC.COMMA;
                case '.':
                    return SC.DOT;
                case '+':
                    return SC.PLUS;
                case '-':
                    return SC.MINUS;
                case '*':
                    return SC.STAR;
                default:
                    throw new NotSupportedException($"Could not convert char '{c}' to special character.");
            }
        }

        //For error messages
        private string SC2String(SC sc)
        {
            switch (sc)
            {
                case SC.A:
                    return "A";
                case SC.B:
                    return "B";
                case SC.E:
                    return "E";
                case SC.G:
                    return "G";
                case SC.N:
                    return "N";
                case SC.P:
                    return "P";
                case SC.S:
                    return "S";
                case SC.V:
                    return "V";
                case SC.X:
                    return "X";
                case SC.Z:
                    return "Z";
                case SC.NINE:
                    return "9";
                case SC.ZERO:
                    return "0";
                case SC.SLASH:
                    return "/";
                case SC.COMMA:
                    return _decimalPointIsComma ? "." : ",";
                case SC.DOT:
                    return _decimalPointIsComma ? "," : ".";
                case SC.PLUS:
                    return "+";
                case SC.MINUS:
                    return "-";
                case SC.STAR:
                    return "*";
                case SC.CS:
                    System.Diagnostics.Debug.Assert(_currencyDescriptor != null);
                    return _currencyDescriptor.Symbol.ToString();
                case SC.CR:
                    return "CR";
                case SC.DB:
                    return "DB";
                default:
                    throw new NotSupportedException($"Unknown '{sc}' special character.");
            }
        }
    }
}

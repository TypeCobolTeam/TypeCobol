using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Integer, Decimal or Floating Point value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxNumber
    {
        public SyntaxNumber(Token token)
        {
            Token = token;
        }

        /// <summary>
        /// Token defining the number value
        /// </summary>
        public Token Token { get; private set; }

        /// <summary>
        /// Numeric value defined by the Token
        /// </summary>
        public object Value
        {
            get
            {
                if (Token.TokenType == TokenType.IntegerLiteral)
                {
                    return ((IntegerLiteralValue)Token.LiteralValue).Number;
                } else
                if (Token.TokenType == TokenType.DecimalLiteral)
                {
                    return ((DecimalLiteralValue)Token.LiteralValue).Number;
                } else
                if (Token.TokenType == TokenType.FloatingPointLiteral)
                {
                    return ((FloatingPointLiteralValue)Token.LiteralValue).Number;
                }
                throw new InvalidOperationException("No numeric value can be defined by a token of type : " + Token.TokenType);
            }
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Value.ToString();
        }
    }

    /// <summary>
    /// Zero defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxZero : SyntaxNumber
    {
        public SyntaxZero(Token token) : base(token) { }

        /// <summary>
        /// 0 as a long
        /// </summary>
        public long Value
        {
            get
            {
                return 0;
            }
        }

        /// <summary>
        /// Debug string "0"
        /// </summary>
        public override string ToString()
        {
            return "0";
        }
    }
}

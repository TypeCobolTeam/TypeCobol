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
                    return ((IntegerLiteralTokenValue)Token.LiteralValue).Number;
                } else
                if (Token.TokenType == TokenType.DecimalLiteral)
                {
                    return ((DecimalLiteralTokenValue)Token.LiteralValue).Number;
                } else
                if (Token.TokenType == TokenType.FloatingPointLiteral)
                {
                    return ((FloatingPointLiteralTokenValue)Token.LiteralValue).Number;
                } else
                if (Token.TokenType == TokenType.ZERO
                 || Token.TokenType == TokenType.ZEROS
                 || Token.TokenType == TokenType.ZEROES)
                {
                    return 0;
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
}

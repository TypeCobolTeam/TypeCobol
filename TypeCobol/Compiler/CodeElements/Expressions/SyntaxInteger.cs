using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Integer value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxInteger
    {
        public SyntaxInteger(Token token)
        {
            Token = token;
        }

        /// <summary>
        /// Token defining the integer value
        /// </summary>
        public Token Token { get; private set; }

        /// <summary>
        /// Integer value defined by the Token
        /// </summary>
        public long Value
        {
            get
            {
                if (Token.TokenType == TokenType.IntegerLiteral)
                {
                    return ((IntegerLiteralValue)Token.LiteralValue).Number;
                }
                else
                {
                    throw new InvalidOperationException("An integer value can not be defined by a token of type : " + Token.TokenType);
                }
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

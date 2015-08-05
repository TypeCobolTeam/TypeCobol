using System;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// String value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxString
    {
        public SyntaxString(Token token)
        {
            Token = token;
        }

        /// <summary>
        /// Token defining the string value
        /// </summary>
        public Token Token { get; private set; }

        /// <summary>
        /// String value defined by the Token
        /// </summary>
        public string Value
        {
            get
            {
                if (Token.TokenFamily == TokenFamily.Symbol)
                {
                    return Token.Text;
                }
                else if (Token.TokenFamily == TokenFamily.AlphanumericLiteral)
                {
                    return Token.Text;
//                    return ((AlphanumericLiteralValue)Token.LiteralValue).Text;
                }
                else
                {
                    throw new InvalidOperationException("A string value can not be defined by a token of type : " + Token.TokenType);
                }
            }
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Value;
        }

        internal static SyntaxString Create(Parser.Generated.CobolCodeElementsParser.AlphanumOrNationalLiteralContext context)
        {
            bool all = context.ALL() != null;//TODO
            if (context.NullTerminatedAlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.NullTerminatedAlphanumericLiteral()));
            }
            if (context.alphanumOrNationalLiteralBase() != null)
            {
                return Create(context.alphanumOrNationalLiteralBase());
            }
            throw new InvalidOperationException("This is not a string!");
        }

        private static SyntaxString Create(Parser.Generated.CobolCodeElementsParser.AlphanumOrNationalLiteralBaseContext context)
        {
            if (context.AlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.AlphanumericLiteral()));
            }
            if (context.HexadecimalAlphanumericLiteral() != null)
            {
                return new SyntaxString(ParseTreeUtils.GetTokenFromTerminalNode(context.HexadecimalAlphanumericLiteral()));
            }
            throw new InvalidOperationException("This is not a string!");
        }
    }
}

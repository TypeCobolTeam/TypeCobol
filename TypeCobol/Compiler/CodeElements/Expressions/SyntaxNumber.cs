using System;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser.Generated;
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

        public static SyntaxNumber Create(CobolCodeElementsParser.NumericLiteralContext context)
        {
            if (context.IntegerLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
            }
            if (context.DecimalLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.DecimalLiteral()));
            }
            if (context.FloatingPointLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.FloatingPointLiteral()));
            }
            if (context.ZERO() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZERO()));
            }
            if (context.ZEROS() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROS()));
            }
            if (context.ZEROES() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROES()));
            }
            throw new InvalidOperationException("This is not a number!");
        }
    }
}

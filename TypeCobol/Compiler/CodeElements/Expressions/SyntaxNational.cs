using System;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Integer, Decimal or Floating Point value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxNational
    {
        public SyntaxNational(Token token)
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
                if (Token.TokenType == TokenType.NationalLiteral)
                {
                    //TODO
                } else
                if (Token.TokenType == TokenType.HexadecimalNationalLiteral)
                {
                    //TODO
                } 
                throw new InvalidOperationException("No national value can be defined by a token of type : " + Token.TokenType);
            }
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Value.ToString();
        }

        public static SyntaxNational Create(CobolCodeElementsParser.AlphanumOrNationalLiteralBaseContext context)
        {
            if (context.NationalLiteral() != null)
            {
                return new SyntaxNational(ParseTreeUtils.GetTokenFromTerminalNode(context.NationalLiteral()));
            }
            if (context.HexadecimalNationalLiteral() != null)
            {
                return new SyntaxNational(ParseTreeUtils.GetTokenFromTerminalNode(context.HexadecimalNationalLiteral()));
            }
            throw new InvalidOperationException("This is not a national!");
        }
    }
}

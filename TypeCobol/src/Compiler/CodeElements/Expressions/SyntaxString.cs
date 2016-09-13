using System;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// String value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxString : Expression
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
                if (Token.TokenFamily == TokenFamily.Symbol ||
                    Token.TokenFamily == TokenFamily.SyntaxKeyword)
                {
                    return Token.Text;
                }
                else if (Token.TokenFamily == TokenFamily.AlphanumericLiteral)
                {
                    return Token.Text;
//                    return ((AlphanumericLiteralValue)Token.LiteralValue).Text;
                }
                else if (Token.TokenFamily == TokenFamily.FigurativeConstantKeyword)
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
    }
}

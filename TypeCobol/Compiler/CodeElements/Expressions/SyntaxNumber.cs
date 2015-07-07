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
    }

    /// <summary>
    /// Integer value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxInteger : SyntaxNumber
    {
        public SyntaxInteger(Token token)
            : base(token)
        { }

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

    /// <summary>
    /// Decimal value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxDecimal : SyntaxNumber
    {
        public SyntaxDecimal(Token token)
            : base(token)
        { }

        /// <summary>
        /// Integer value defined by the Token
        /// </summary>
        public double Value
        {
            get
            {
                if (Token.TokenType == TokenType.DecimalLiteral)
                {
                    return ((DecimalLiteralValue)Token.LiteralValue).Number;
                }
                else
                {
                    throw new InvalidOperationException("A decimal value can not be defined by a token of type : " + Token.TokenType);
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

    /// <summary>
    /// Decimal value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxFloat : SyntaxNumber
    {
        public SyntaxFloat(Token token)
            : base(token)
        { }

        /// <summary>
        /// Integer value defined by the Token
        /// </summary>
        public double Value
        {
            get
            {
                if (Token.TokenType == TokenType.FloatingPointLiteral)
                {
                    return ((DecimalLiteralValue)Token.LiteralValue).Number;
                }
                else
                {
                    throw new InvalidOperationException("A floating point value can not be defined by a token of type : " + Token.TokenType);
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

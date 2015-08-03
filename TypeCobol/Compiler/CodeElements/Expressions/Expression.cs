using System;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{

    public abstract class Expression { }

    public class Literal : Expression
    {
        private SyntaxNumber numberValue;
        private SyntaxString stringValue;

        public Literal(Parser.Generated.CobolCodeElementsParser.LiteralContext context)
        {
            if (context.numericLiteral() != null)
            {
                numberValue = SyntaxNumber.Create(context.numericLiteral());
            }
            if (context.alphanumOrNationalLiteral() != null)
            {
                stringValue = SyntaxString.Create(context.alphanumOrNationalLiteral());
            }
        }

        /// <summary>
        /// Numeric or string value defined by the Token
        /// </summary>
        public object Value
        {
            get
            {
                if (numberValue != null)
                {
                    return numberValue.Value;
                }
                if (stringValue != null)
                {
                    return stringValue.Value;
                }
                throw new InvalidOperationException("Malformed literal");
            }
        }

        public override string ToString()
        {
            if (numberValue != null)
            {
                return numberValue.ToString();
            }
            if (stringValue != null)
            {
                return stringValue.ToString();
            }
            throw new InvalidOperationException("Malformed literal");
        }
    }


    //TODO this class is only use for display literals for now.
    //osmedile: I'm not sure if literal must inherits from Expression (so Literal and identifier can be seen as the same object) 
    //if this class can inherits from Expression, rename it to "Literal"
    public class LiteralForDisplay : Expression
    {
        public Token token { get; set; }
        public LiteralForDisplay(Token token)
        {
            this.token = token;
        }
        public override string ToString() { return token.Text; }
    }

}

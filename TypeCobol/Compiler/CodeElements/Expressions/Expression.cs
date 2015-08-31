using System;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{

    public interface Expression { }

    public class Literal : Expression
    {
        private SyntaxNumber numberValue;
        private SyntaxString stringValue;
        public bool All = false;

        public Literal(SyntaxNumber numberValue)
        {
            this.numberValue = numberValue;
        }

        public Literal(SyntaxString stringValue)
        {
            this.stringValue = stringValue;
        }

        /// <summary>
        /// Numeric or string value defined by the Token
        /// </summary>
        public object Value
        {
            get
            {
                if (numberValue != null) return numberValue.Value;
                if (stringValue != null) return stringValue.Value;
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

}

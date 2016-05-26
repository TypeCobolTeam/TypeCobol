using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Property defined by a keyword token in the Cobol syntax
    /// </summary>
    public class SyntaxProperty<T>
    {
        public SyntaxProperty(T value, Token token)
        {
            Value = value;
            Token = token;
        }

        /// <summary>
        /// Value of the property 
        /// </summary>
        public T Value { get; private set; }

        /// <summary>
        /// Token defining the value of the property
        /// </summary>
        public Token Token { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Value.ToString();
        }
    }
}

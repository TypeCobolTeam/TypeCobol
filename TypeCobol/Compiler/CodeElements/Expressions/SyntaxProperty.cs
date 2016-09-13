using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Property defined by a list of tokens in the Cobol syntax
    /// </summary>
    public class SyntaxProperty<T>
    {
        public SyntaxProperty(T value, IList<Token> tokens)
        {
            Value = value;
            Tokens = tokens;
        }

        /// <summary>
        /// Value of the property 
        /// </summary>
        public T Value { get; private set; }

        /// <summary>
        /// Tokens defining the value of the property
        /// </summary>
        public IList<Token> Tokens { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Value.ToString();
        }
    }
}

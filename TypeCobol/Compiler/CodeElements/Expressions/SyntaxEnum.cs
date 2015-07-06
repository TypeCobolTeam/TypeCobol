using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Boolean value defined by a single Token in the Cobol syntax
    /// </summary>
    public class SyntaxEnum<E>
    {
        public SyntaxEnum(Token token, E envName)
        {
            Token = token;
            EnumValue = envName;
        }

        /// <summary>
        /// Token used to define the value of EnvironmentName enum
        /// </summary>
        public Token Token { get; private set; }   
        
        /// <summary>
        /// Enum value
        /// </summary>
        public E EnumValue { get; private set; }

        /// <summary>
        /// Value defined by the token
        /// </summary>
        public E Value
        {
            get { return EnumValue; }
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

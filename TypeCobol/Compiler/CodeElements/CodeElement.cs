using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Common properties shared between all code elements
    /// </summary>
    public abstract class CodeElement
    {
        public CodeElement(CodeElementType type)
        {
            Type = type;
        }

        /// <summary>
        /// The Cobol syntax can be decomposed in 116 elementary code elements
        /// </summary>
        public CodeElementType Type { get; private set; }

        /// <summary>
        /// All significant tokens consumed in the source document to build this code element
        /// </summary>
        public IList<Token> ConsumedTokens { get; set; }

        /// <summary>
        /// Line index in the main document where the first consumed token is starting
        /// </summary>
        public int FirstTokenLineIndexInMainDocument { get; set; }

        /// <summary>
        /// Line index in the main document where the last consumed token is starting
        /// </summary>
        public int LastTokenLineIndexInMainDocument { get; set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return "[" + Type.ToString() + " : "+ ConsumedTokens[0].ToString() +" --> " + ConsumedTokens[ConsumedTokens.Count-1].ToString() + "]";
        }
    }
}

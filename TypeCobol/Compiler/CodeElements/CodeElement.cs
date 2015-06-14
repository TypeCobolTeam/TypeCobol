using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Common properties shared between all code elements.
    /// A CodeElement produced during the first parsing phase is also a token consumed by the second parsing phase. 
    /// </summary>
    public abstract class CodeElement : IToken
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
            StringBuilder sb = new StringBuilder("[[");
            sb.Append(Type);
            sb.Append("]] ");
            sb.Append(ConsumedTokens[0]).ToString();
            sb.Append(" --> ");
            sb.AppendLine(ConsumedTokens[ConsumedTokens.Count - 1].ToString());
            return sb.ToString();
        }

        // --- Antlr4.Runtime.IToken implementation ---
        // ... used by the CobolProgramClassParser  ...

        public string Text
        {
            get
            {
                StringBuilder sb = new StringBuilder();
                foreach(Token elementToken in ConsumedTokens)
                {
                    sb.Append(elementToken.Text);
                }
                return sb.ToString();
            }
        }

        int IToken.Type
        {
            get
            {
                return (int)Type;
            }
        }

        public int Line
        {
            get
            {
                return ConsumedTokens[0].Line;
            }
        }

        public int Column
        {
            get
            {
                return ConsumedTokens[0].Column;
            }
        }

        public int Channel
        {
            get
            {
                return 1;
            }
        }

        public int TokenIndex
        {
            get
            {
                return ConsumedTokens[0].TokenIndex;
            }
        }

        public int StartIndex
        {
            get
            {
                return ConsumedTokens[0].StartIndex;
            }
        }

        public int StopIndex
        {
            get
            {
                return ConsumedTokens[ConsumedTokens.Count-1].StopIndex; ;
            }
        }

        public ITokenSource TokenSource
        {
            get
            {
                return ConsumedTokens[0].TokenSource;
            }
        }

        public ICharStream InputStream
        {
            get
            {
                return ConsumedTokens[0].InputStream;
            }
        }
    }
}

using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Symbols;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Common properties shared between all code elements.
    /// A CodeElement produced during the first parsing phase is also a token consumed by the second parsing phase. 
    /// </summary>
    public abstract partial class CodeElement: IToken
    {
        public CodeElement(CodeElementType type)
        {
            Type = type;
            SymbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
            Diagnostics = new List<Diagnostic>();
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
        /// Is the token is a UserDefinedWord or a literal, it could be a symbol definition or a symbol reference.
        /// Keywords can also be symbol references (special registers).
        /// This property enables to retrieve symbol information attached to this token at a later stage.
        /// </summary>
        public IDictionary<Token,SymbolInformation> SymbolInformationForTokens { get; set; }
        
        /// <summary>
        /// List of errors found on this CodeElement
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }
        
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
            sb.Append(ConsumedTokens[ConsumedTokens.Count - 1].ToString());
            bool displayLineNumbers = false;
            if (displayLineNumbers)
            {
                int first = ConsumedTokens[0].Line;
                int last  = ConsumedTokens[ConsumedTokens.Count-1].Line;
                sb.Append(" on lines ");
                sb.Append(first);
                sb.Append(">");
                sb.Append(last);
            }
            sb.AppendLine(); //TODO: is the newline really necessary here ? ToString returns shouldn't end with a newline, should they ?
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
                return -1;
                //return ConsumedTokens[0].Line;
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
                return Token.CHANNEL_SourceTokens;
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

        public CodeElement Parent {
            get;
            internal set;
        }
    }
}

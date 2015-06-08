using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Reference to a symbol in the Cobol syntax
    /// </summary>
    public class SymbolReference<S> where S : Symbol
    {
        public SymbolReference(S symbolReference)
        {
            Symbol = symbolReference;
        }

        /// <summary>
        /// Reference to a symbol in the source text
        /// </summary>
        public S Symbol { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Symbol.Name;
        }
    }

    /// <summary>
    /// In several productions, the Cobol grammar expects either a literal value or a symbol reference
    /// </summary>
    public class LiteralOrSymbolReference<L, S> where S : Symbol
    {
        /// <summary>
        /// Constructor for a literal value
        /// </summary>
        public LiteralOrSymbolReference(L literalValue)
        {
            IsLiteral = true;
            LiteralValue = literalValue;
        }

        /// <summary>
        /// Constructor for a symbol reference
        /// </summary>
        public LiteralOrSymbolReference(S symbolReference)
        {
            IsLiteral = false;
            SymbolReference = symbolReference;
        }

        /// <summary>
        /// True if the developer specified a literal value.
        /// False if the developer specified a symbol reference.
        /// </summary>
        public bool IsLiteral { get; private set; }

        /// <summary>
        /// Literal value
        /// </summary>
        public L LiteralValue { get; private set; }

        /// <summary>
        /// Symbol reference
        /// </summary>
        public S SymbolReference { get; private set; }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if(IsLiteral)
            {
                return LiteralValue.ToString();
            }
            else
            {
                return SymbolReference.ToString();
            }
        }
    }
}

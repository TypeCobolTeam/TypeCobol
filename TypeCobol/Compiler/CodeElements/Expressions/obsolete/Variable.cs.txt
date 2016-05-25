using System;

namespace TypeCobol.Compiler.CodeElements.Obsolete
    /// <summary>
    /// In several productions, the Cobol grammar expects either a literal value or a symbol reference
    /// </summary>
    public class LiteralOrSymbolReference<L, S> where S : SymbolReference
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
            if (IsLiteral)
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

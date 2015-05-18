using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public class SymbolReference<S> where S : Symbol
    {
        public SymbolReference(S symbolReference)
        {
            Symbol = symbolReference;
        }

        public S Symbol { get; private set; }
    }

    public class LiteralOrSymbolReference<L, S> where S : Symbol
    {
        public LiteralOrSymbolReference(L literalValue)
        {
            IsLiteral = true;
            LiteralValue = literalValue;
        }

        public LiteralOrSymbolReference(S symbolReference)
        {
            IsLiteral = false;
            SymbolReference = symbolReference;
        }

        public bool IsLiteral { get; private set; }

        public L LiteralValue { get; private set; }

        public S SymbolReference { get; private set; }
    }
}

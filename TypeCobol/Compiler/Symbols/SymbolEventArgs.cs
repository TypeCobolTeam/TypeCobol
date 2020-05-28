using System;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// An EventArgs for a symbol
    /// </summary>
    public class SymbolEventArgs : EventArgs
    {
        /// <summary>
        /// The underlying symbol
        /// </summary>
        public Symbol Symbol
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="symbol">The Symbol</param>
        public SymbolEventArgs(Symbol symbol)
        {
            Symbol = symbol;
        }
    }
}

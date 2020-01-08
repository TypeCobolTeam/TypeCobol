using System.Collections;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// A Scope associates a set of symbols (a domain) with an owner.
    /// </summary>
    public class Scope<TSymbol> : IEnumerable<TSymbol>
        where TSymbol : Symbol
    {
        /// <summary>
        /// The symbols declared in this scope.
        /// </summary>
        private Domain<TSymbol> _symbols;

        /// <summary>
        /// The Owner of this scope.
        /// </summary>
        public Symbol Owner { get; private set; }

        /// <summary>
        /// Instantiates a new empty scope.
        /// </summary>
        /// <param name="owner">The owner of the new scope.</param>
        public Scope(Symbol owner)
        {
            Owner = owner;
        }

        /// <summary>
        /// Looks up a Symbol in this scope using a name.
        /// </summary>
        /// <param name="name">Name of the symbol searched.</param>
        /// <returns>An entry if any symbol with the given name has been found, null otherwise.</returns>
        public Domain<TSymbol>.Entry Lookup([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);
            if (_symbols != null && _symbols.TryGetValue(name, out var entry))
            {
                return entry;
            }

            return null;
        }

        /// <summary>
        /// Adds a symbol in this scope.
        /// </summary>
        /// <param name="symbol">Symbol to add.</param>
        /// <returns>The domain entry corresponding to the symbol.</returns>
        public Domain<TSymbol>.Entry Enter([NotNull] TSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            if (_symbols == null)
            {
                _symbols = new Domain<TSymbol>();
            }

            return _symbols.Add(symbol);
        }

        /// <summary>
        /// Removes a symbol from this scope.
        /// </summary>
        /// <param name="symbol">Symbol to remove.</param>
        public void Delete([NotNull] TSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            _symbols?.Remove(symbol);
        }

        /// <summary>
        /// Enumerates all symbols in this scope in entering order.
        /// </summary>
        /// <returns></returns>
        public IEnumerator<TSymbol> GetEnumerator()
        {
            return _symbols != null ? _symbols.GetEnumerator() : Enumerable.Empty<TSymbol>().GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        /// <summary>
        /// Change the Owner of this scope, and does it for all symbols.
        /// </summary>
        /// <param name="owner">The new Owner</param>
        public void ChangeOwner(Symbol owner)
        {
            this.Owner = owner;
            if (_symbols != null)
            {
                foreach (var symbol in _symbols)
                {
                    symbol.Owner = owner;
                }
            }
        }
    }
}

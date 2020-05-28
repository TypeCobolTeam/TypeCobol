using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// A Domain associates a set of symbols with an owner.
    /// </summary>
    public class Domain<TSymbol> : IEnumerable<TSymbol>
        where TSymbol : Symbol
    {
        /// <summary>
        /// The Owner of this domain.
        /// </summary>
        private Symbol _owner;

        /// <summary>
        /// The symbols declared in this domain.
        /// </summary>
        private Container<TSymbol> _symbols;

        /// <summary>
        /// Instantiates a new empty domain.
        /// </summary>
        /// <param name="owner">The owner of the new domain.</param>
        public Domain([NotNull] Symbol owner)
        {
            System.Diagnostics.Debug.Assert(owner != null);
            _owner = owner;
        }

        /// <summary>
        /// The Owner of this domain.
        /// </summary>
        [NotNull]
        public Symbol Owner
        {
            get => _owner;
            set
            {
                System.Diagnostics.Debug.Assert(value != null);
                _owner = value;
                if (_symbols != null)
                {
                    foreach (var symbol in _symbols)
                    {
                        symbol.Owner = _owner;
                    }
                }
            }
        }

        /// <summary>
        /// Looks up a Symbol in this domain using a name.
        /// </summary>
        /// <param name="name">Name of the symbol searched.</param>
        /// <returns>An entry if any symbol with the given name has been found, null otherwise.</returns>
        public Container<TSymbol>.Entry Lookup([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);
            if (_symbols != null && _symbols.TryGetValue(name, out var entry))
            {
                return entry;
            }

            return null;
        }

        /// <summary>
        /// Adds a symbol in this domain.
        /// </summary>
        /// <param name="symbol">Symbol to add.</param>
        /// <returns>The container entry corresponding to the symbol.</returns>
        public Container<TSymbol>.Entry Enter([NotNull] TSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            if (_symbols == null)
            {
                _symbols = new Container<TSymbol>();
            }

            return _symbols.Add(symbol);
        }

        /// <summary>
        /// Removes a symbol from this domain.
        /// </summary>
        /// <param name="symbol">Symbol to remove.</param>
        public void Delete([NotNull] TSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            _symbols?.Remove(symbol);
        }

        /// <summary>
        /// Enumerates all symbols in this domain in entering order.
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
        /// The count of symbols in this domain
        /// </summary>
        public int Count => _symbols.Count;

        /// <summary>
        /// Indexed accessor
        /// </summary>
        /// <param name="i">Index of the symbol to get</param>
        /// <returns>The Symbol at the given index</returns>
        /// <exception cref="IndexOutOfRangeException">The given index is out of range</exception>
        public TSymbol this[int i]
        {
            get
            {
                if (_symbols == null)
                    throw new IndexOutOfRangeException();
                return _symbols[i];
            }
        }
    }
}

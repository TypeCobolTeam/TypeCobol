using System;
using System.Collections;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// A domain is a set of symbols.
    /// </summary>
    /// <typeparam name="TSymbol"></typeparam>
    public class Domain<TSymbol> : IEnumerable<TSymbol>
        where TSymbol : Symbol
    {
        /// <summary>
        /// Represents an entry in the domain. Regroups all symbols associated with a name.
        /// </summary>
        public interface IEntry : IEnumerable<TSymbol>
        {
            /// <summary>
            /// Indicates whether this entry is unique or multiple.
            /// </summary>
            bool IsUnique { get; }

            /// <summary>
            /// Main Symbol associated to this entry.
            /// </summary>
            [NotNull] TSymbol Symbol { get; }
        }

        /// <summary>
        /// Implements IEntry with an optimization : an entry is created as unique
        /// and we switch to a List implementation only if a second symbol is added.
        /// </summary>
        private class Entry : IEntry
        {
            private readonly TSymbol _primarySymbol;
            private List<TSymbol> _symbols;

            public Entry(TSymbol symbol)
            {
                _primarySymbol = symbol;
            }

            public bool IsUnique => _symbols == null || _symbols.Count == 1;

            public TSymbol Symbol => _symbols != null ? _symbols[0] : _primarySymbol;

            public IEnumerator<TSymbol> GetEnumerator()
            {
                if (_symbols != null)
                {
                    foreach (var symbol in _symbols)
                    {
                        yield return symbol;
                    }
                }
                else
                {
                    yield return _primarySymbol;
                }
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            /// <summary>
            /// Adds a symbol to this entry.
            /// </summary>
            /// <param name="symbol">Symbol to add.</param>
            public void Add([NotNull] TSymbol symbol)
            {
                System.Diagnostics.Debug.Assert(symbol != null);
                if (_symbols == null)
                {
                    _symbols = new List<TSymbol>() {_primarySymbol};
                }

                _symbols.Add(symbol);
            }

            /// <summary>
            /// Removes a Symbol from this entry.
            /// </summary>
            /// <param name="symbol">Symbol to remove.</param>
            /// <returns>
            /// True if the entry is not valid anymore (i.e. empty), False otherwise.
            /// Caller must delete the entry if this method returns True.
            /// </returns>
            public bool Remove([NotNull] TSymbol symbol)
            {
                System.Diagnostics.Debug.Assert(symbol != null);
                if (_symbols == null)
                {
                    return _primarySymbol == symbol;
                }

                _symbols.Remove(symbol);
                return _symbols.Count == 0;
            }
        }

        private readonly Dictionary<string, Entry> _symbols;
        private readonly List<TSymbol> _symbolsInOrder;

        /// <summary>
        /// Creates an empty domain.
        /// </summary>
        public Domain()
        {
            _symbols = new Dictionary<string, Entry>(StringComparer.OrdinalIgnoreCase);
            _symbolsInOrder = new List<TSymbol>();
        }

        /// <summary>
        /// Creates a domain from an existing one.
        /// </summary>
        /// <param name="other">Other domain to copy symbols from.</param>
        public Domain([NotNull] Domain<TSymbol> other)
        {
            System.Diagnostics.Debug.Assert(other != null);
            _symbols = new Dictionary<string, Entry>(other._symbols, StringComparer.OrdinalIgnoreCase);
            _symbolsInOrder = new List<TSymbol>(other._symbolsInOrder);
        }

        public IEnumerator<TSymbol> GetEnumerator()
        {
            return _symbolsInOrder.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        /// <summary>
        /// Searches an entry in this domain according to a name.
        /// </summary>
        /// <param name="name">Name of searched Symbol.</param>
        /// <param name="entry">Result of the search.</param>
        /// <returns>True if an entry has been found, False otherwise.</returns>
        public bool TryGetValue([NotNull] string name, out IEntry entry)
        {
            System.Diagnostics.Debug.Assert(name != null);
            if (_symbols.TryGetValue(name, out var editableEntry))
            {
                entry = editableEntry;
                return true;
            }

            entry = null;
            return false;
        }

        /// <summary>
        /// Adds a Symbol to this domain.
        /// </summary>
        /// <param name="symbol">Symbol to add.</param>
        public void Add([NotNull] TSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            string key = symbol.Name;
            if (_symbols.TryGetValue(key, out var entry))
            {
                //entry already exists, add inside it.
                entry.Add(symbol);
            }
            else
            {
                //create a new entry for this symbol.
                _symbols.Add(key, new Entry(symbol));
            }

            _symbolsInOrder.Add(symbol);
        }

        /// <summary>
        /// Removes a Symbol from this domain.
        /// </summary>
        /// <param name="symbol">Symbol to remove.</param>
        public void Remove([NotNull] TSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            string key = symbol.Name;
            if (_symbols.TryGetValue(key, out var entry))
            {
                if (entry.Remove(symbol))
                {
                    //Entry is no longer valid, remove it from the dictionary.
                    _symbols.Remove(key);
                }

                _symbolsInOrder.Remove(symbol);
            }
        }
    }
}

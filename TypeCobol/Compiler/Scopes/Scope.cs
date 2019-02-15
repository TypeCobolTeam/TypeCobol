using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// A Cobol Scope which contains Cobol Symbols declared within a Scope
    /// </summary>
    public class Scope<T> : IEnumerable<T> where T : Symbol
    {
        /// <summary>
        /// A Symbol Entry in this scope
        /// </summary>
        public class Entry : IEnumerable<T>
        {
            /// <summary>
            /// The symbol
            /// </summary>
            public  T Symbol
            {
                get;
                protected set;
            }

            /// <summary>
            /// The count of symbol
            /// </summary>
            public virtual int Count => 1;

            /// <summary>
            /// Indexer
            /// </summary>
            /// <param name="i"></param>
            /// <returns></returns>
            public virtual T this [int i]
            {
                get
                {
                    System.Diagnostics.Debug.Assert(i == 0);
                    return i == 0 ? Symbol : null;
                }
            }

            /// <summary>
            /// Check this symbol exists in this entry
            /// </summary>
            /// <param name="sym">The symbol to check</param>
            /// <returns>true if yes, false otherwise</returns>
            public virtual bool Exits(T sym)
            {
                foreach (var t in this)
                {
                    if (t == sym)
                    {
                        return true;
                    }
                }
                return false;
            }

            /// <summary>
            /// Enumerator on symbols
            /// </summary>
            /// <returns></returns>
            public IEnumerator<T> GetEnumerator()
            {
                for (int i = 0; i < Count; i++)
                {
                    yield return this[i];
                }
            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        /// <summary>
        /// One Symbol constructor.
        /// </summary>
        /// <param name="symbol"></param>
        public Entry(T symbol)
            {
                System.Diagnostics.Debug.Assert(symbol != null);
                Symbol = symbol;
            }
        protected Entry()
            {

            }
        }

        /// <summary>
        /// A Multi symbol entry class that represent several symbol.
        /// </summary>
        public class MultiSymbols : Entry
        {
            /// <summary>
            /// All Symbols
            /// </summary>
            private List<T> _symbols = new List<T>();
            /// <summary>
            /// Empty constructor
            /// </summary>
            public MultiSymbols()
            {                
            }

            /// <summary>
            /// Constructor with at list one symbol.
            /// </summary>
            public MultiSymbols(T symbol) : base(symbol)
            {
                _symbols.Add(symbol);
            }

            /// <summary>
            /// Add a symbol
            /// </summary>
            /// <param name="symbol">The symbol to be added</param>
            public void Add(T symbol)
            {
                if (Count == 0)
                {
                    base.Symbol = symbol;
                }
                _symbols.Add(symbol);
            }

            /// <summary>
            /// The count of symbol
            /// </summary>
            public override int Count => _symbols.Count;

            public override T this[int i] => _symbols[i];

            /// <summary>
            /// Remove the given symbol in this multy entries
            /// </summary>
            /// <param name="sym"></param>
            internal void Remove(T sym)
            {
                //After removing a multi must at least contains one element.
                System.Diagnostics.Contracts.Contract.Ensures(Count >= 1);
                _symbols.Remove(sym);                
                //base.Symbol is always the first symbol of the list.
                Symbol = _symbols[0];
            }
        }


        /// <summary>
        /// 
        /// The Owner of this scope
        /// </summary>
        public Symbol Owner
        {
            get;
            set;
        }
        /// <summary>
        /// All Symbols in this scope, each symbol is associated an integer number which is
        /// its index in the _OrderedSymbols list.
        /// </summary>
        Dictionary<String, Entry > _symbols = null;
        /// <summary>
        /// All symbol in enter order.
        /// </summary>
        List<T> _orderedSymbols = null;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="owner">Scope Owner</param>
        public Scope(Symbol owner)
        {
            this.Owner = owner;
        }

        /// <summary>
        /// Enter a Symbol in this scope, if a previous symbol with the same name exits, then a MultySymbols entry
        /// will be used to store the nes symbol.
        /// </summary>
        /// <param name="sym">The Symbol to enter</param>
        /// <returns>The entry of the symbol</returns>
        public Entry Enter(T sym)
        {
            System.Diagnostics.Debug.Assert(sym != null);
            System.Diagnostics.Debug.Assert(sym.Name != null);
            if (_symbols == null)
            {
                _symbols = new Dictionary<String, Entry >();
                _orderedSymbols = new List<T>();
            }
            Entry entry = null;
            //String name = String.Intern(sym.Name.ToLower());
            String name = sym.Name.ToLower();
            if (!_symbols.ContainsKey(name))
            {
                _symbols[name] = entry = new Entry(sym);
            }
            else
            {
                //Check if the symbol itself exits                
                entry = _symbols[name];
                if (entry.Exits(sym))
                    return entry;
                MultiSymbols multi = null;
                if (entry.Count == 1)
                {//Create a multi symbols entry
                    multi = new MultiSymbols(entry.Symbol);                    
                    _symbols[name] = multi;
                }
                else
                {
                    multi = _symbols[name] as MultiSymbols;
                }
                multi.Add(sym);
                entry = multi;
            }
            _orderedSymbols.Add(sym);
            return entry;
        }

        /// <summary>
        /// Enter symbol sym in this scope if not already exists.
        /// </summary>
        /// <param name="sym">The symbol to enter</param>
        public void EnterIfNotExist(T sym)
        {
            System.Diagnostics.Contracts.Contract.Requires(sym != null);
            Entry entry = Lookup(sym.Name);
            if (entry != null)
                Enter(sym);
        }

        /// <summary>
        /// Remove the given symbol
        /// </summary>
        /// <param name="sym">The symbol to be removed</param>
        public void Remove(T sym)
        {
            System.Diagnostics.Contracts.Contract.Requires(sym != null);
            if (sym == null)
                return;
            String name = sym.Name.ToLower();
            Entry entry = null;
            if (_symbols != null)
            {
                _symbols.TryGetValue(name.ToLower(), out entry);
                if (entry != null)
                {
                    if (entry.Count == 1)
                    {
                        _symbols.Remove(name);
                    }
                    else
                    {//This is a multi symbol entry
                        System.Diagnostics.Debug.Assert(entry is MultiSymbols);
                        MultiSymbols multi = (MultiSymbols)entry;
                        multi.Remove(sym);
                        if (multi.Count == 1)
                        {//Ensure the invariant, it is no longer a multi.
                            _symbols[name] = new Entry(multi.Symbol);
                        }
                    }
                    //Remove it from the ordered list
                    this._orderedSymbols.Remove(sym);
                }
            }
        }

        /// <summary>
        /// Lookup a symbol in this scope
        /// </summary>
        /// <param name="name">The Symbol's name</param>
        /// <returns>The Entry of Symbol if it exist, null otherwise</returns>
        public Entry Lookup(String name)
        {
            System.Diagnostics.Contracts.Contract.Requires(name != null);
            if (name == null)
                return null;

            Entry entry = null;
            if (_symbols != null)
            {
                _symbols.TryGetValue(name.ToLower(), out entry);
            }
            return entry;
        }

        /// <summary>
        /// Get the Entry of a symbol with the given name
        /// </summary>
        /// <param name="">The Symbol's name</param>
        /// <returns>The Symbol's Entry if it exists, null otherwise</returns>
        public Entry this[string name] => Lookup(name);

        /// <summary>
        /// Get the symbol at the given index
        /// </summary>
        /// <param name="index">The index symbol</param>
        /// <returns></returns>
        public T this[int index]
        {
            get
            {
                if (_orderedSymbols == null)
                    return null;
                return (index >= 0 && index < _orderedSymbols.Count) ? _orderedSymbols[index] : null;
            }
        }

        /// <summary>
        /// Get the index of a symbol inside the scope.
        /// </summary>
        /// <param name="sym">The symbol to get the index</param>
        /// <returns>An index value >= 0 if the symbol exists, -1 otherwise</returns>
        public int IndexOf(T sym)
        {
            if (_orderedSymbols == null)
                return -1;
            return _orderedSymbols.IndexOf(sym);
        }

        /// <summary>
        /// Does this scope contains the given symbol name?
        /// </summary>
        /// <param name="name">The symbol's name to check.</param>
        /// <returns>true if this scope contains the symbol, false otherwise.</returns>
        public bool Contains(string name)
        {
            return this[name] != null;
        }

        /// <summary>
        /// Enumerate all symbols in this scope in the entering order.
        /// </summary>
        /// <returns>>The symbol enumerator.</returns>
        public IEnumerator<T> GetEnumerator()
        {
            if (_orderedSymbols != null)
            {
                foreach (T sym in _orderedSymbols)
                    yield return sym;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        /// <summary>
        /// Get the count of symbols in this scope.
        /// </summary>
        public int Count => _orderedSymbols == null ? 0 : _orderedSymbols.Count;

        /// <summary>
        /// Get the list of symbols in this scope, in entering order.
        /// </summary>
        internal IList<T> Symbols => _orderedSymbols;

        /// <summary>
        /// Change the Owner of this scope, and does it for all symbols.
        /// </summary>
        /// <param name="owner">The new Owner</param>
        public void ChangeOwner(TypedefSymbol owner)
        {
            this.Owner = owner;
            foreach (var field in this)
            {
                field.Owner = owner;
            }
        }
    }
}

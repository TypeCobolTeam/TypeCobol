using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// The Root Symbol Table is a special Namespace
    /// </summary>
    public class RootSymbolTable : NamespaceSymbol
    {
        /// <summary>
        /// All Kinds of scope that contains symbols (i.e. inheritors of ScopeSymbol except RootSymbolTable).
        /// </summary>
        private static readonly Symbol.Kinds[] _AllScopeKinds = new Kinds[] { Kinds.Namespace, Kinds.Program, Kinds.Function };

        /// <summary>
        /// This is the first variable of the universe that can be assimilated to the 0 or null variable.
        /// </summary>
        public static readonly VariableSymbol BottomVariable = new VariableSymbol("<<BottomVariable>>");

        /// <summary>
        /// The index of the last variable symbol entered in this table.
        /// </summary>
        private int _variableSymbolIndex;

        /// <summary>
        /// A pool of free global index to be reused when entering a new variable.
        /// </summary>
        private readonly Stack<int> _globalIndexPool;

        /// <summary>
        /// All Ordered Symbol that can be reached from this Root Symbol Table.
        /// This is in fact the full list of variables known in this Root Symbol Table.
        /// </summary>
        private readonly IList<VariableSymbol> _universe;

        /// <summary>
        /// All known namespaces, programs and functions.
        /// </summary>
        private readonly Container<ScopeSymbol> _allScopes;

        /// <summary>
        /// All known types.
        /// </summary>
        private readonly Container<TypedefSymbol> _allTypes;

        /// <summary>
        /// Empty Constructor.
        /// </summary>
        public RootSymbolTable()
            : base(string.Intern("<<Root>>"))
        {
            base.Kind = Kinds.Root;

            _variableSymbolIndex = 0;
            _globalIndexPool = new Stack<int>();

            _universe = new List<VariableSymbol>();
            _allScopes = new Container<ScopeSymbol>();
            _allTypes = new Container<TypedefSymbol>();

            //Initialize the universe of variables with BottomVariable
            Store(BottomVariable);

            //Load Builtin symbols
            Types = new Domain<TypedefSymbol>(this);
            foreach (var type in BuiltinSymbols.All)
            {
                //add in the designated built-in types collection and store in allTypes
                Types.Enter(type);
                Store(type);
            }
        }

        /// <summary>
        /// Contains Built-in types.
        /// </summary>
        public override Domain<TypedefSymbol> Types { get; protected set; }

        /// <summary>
        /// Full qualified name of this Symbol à la TypeCobol using "::"
        /// </summary>
        public override string FullName => "";

        /// <summary>
        /// Full qualified name of this Symbol à la COBOL85 using OF
        /// </summary>
        public override string FullOfName => "";

        /// <summary>
        /// Full dotted qualified name
        /// </summary>
        public override string FullDotName => "";

        /// <summary>
        /// Full typed dotted qualified name
        /// </summary>
        public override string FullTypedDotName => "";

        /// <summary>
        /// Name followed by type name.
        /// </summary>
        public override string TypedName => "";

        /// <summary>
        /// Program add event.
        /// </summary>
        public event EventHandler<SymbolEventArgs> ProgramAdded;

        /// <summary>
        /// Get the Next VariableSymbol Context.
        /// </summary>
        /// <returns></returns>
        private int NextVariableSymbolIndex()
        {
            if (_globalIndexPool.Count > 0)
            {
                //Re-use free global index.
                return _globalIndexPool.Pop();
            }

            //Increment global counter
            return _variableSymbolIndex++;
        }

        /// <summary>
        /// Store the given VariableSymbol instance in this Root Symbol Table universe.
        /// </summary>
        /// <param name="varSym">The Variable Symbol to be stored.</param>
        internal void Store(VariableSymbol varSym)
        {
            System.Diagnostics.Debug.Assert(varSym != null);
            System.Diagnostics.Debug.Assert(varSym.GlobalIndex == 0);

            varSym.GlobalIndex = NextVariableSymbolIndex();
            _universe.Add(varSym);
        }

        /// <summary>
        /// Discard the given variable symbol. The variable is removed from the universe
        /// of variables.
        /// </summary>
        /// <param name="varSym">The variable symbol to be discarded.</param>
        internal void Discard(VariableSymbol varSym)
        {
            System.Diagnostics.Debug.Assert(varSym != null);
            System.Diagnostics.Debug.Assert(varSym.GlobalIndex != 0);

            if (varSym.GlobalIndex != 0)
            {
                _universe[varSym.GlobalIndex] = null;
                _globalIndexPool.Push(varSym.GlobalIndex);
                varSym.GlobalIndex = 0;
            }
        }

        /// <summary>
        /// Store the given ScopeSymbol instance in this table.
        /// </summary>
        /// <param name="scope">The scope to be stored.</param>
        public void Store(ScopeSymbol scope)
        {
            System.Diagnostics.Debug.Assert(scope != null);
            _allScopes.Add(scope);
            if (ProgramAdded != null && scope.Kind == Kinds.Program)
                ProgramAdded(this, new SymbolEventArgs(scope));
        }

        /// <summary>
        /// Discard the given ScopeSymbol. The scope is removed
        /// from the list of known scopes.
        /// </summary>
        /// <param name="scope">The scope to be discarded.</param>
        public void Discard(ScopeSymbol scope)
        {
            System.Diagnostics.Debug.Assert(scope != null);
            _allScopes.Remove(scope);
            scope.DiscardSymbolsFromRoot();
        }

        /// <summary>
        /// Store the given Type instance in this table.
        /// </summary>
        /// <param name="type">The type to be stored.</param>
        public void Store(TypedefSymbol type)
        {
            System.Diagnostics.Debug.Assert(type != null);
            _allTypes.Add(type);
        }

        /// <summary>
        /// Discard the given type. The type is removed from the list
        /// of known types.
        /// </summary>
        /// <param name="type">The type to be discarded.</param>
        public void Discard(TypedefSymbol type)
        {
            System.Diagnostics.Debug.Assert(type != null);
            _allTypes.Remove(type);
        }

        /// <summary>
        /// Performs a search in the universe of variables of this RootSymbolTable
        /// using a name.
        /// </summary>
        /// <param name="name">Name of the variable searched.</param>
        /// <returns>A non-null container entry of variables matching the given name.</returns>
        [NotNull]
        public Container<VariableSymbol>.Entry LookupVariable([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);

            var results = new Container<VariableSymbol>.Entry(name);
            foreach (var variableSymbol in _universe.Where(v => string.Equals(v.Name, name, StringComparison.OrdinalIgnoreCase)))
            {
                results.Add(variableSymbol);
            }

            return results;
        }

        /// <summary>
        /// Searches for scopes of this RootSymbolTable having the given name.
        /// </summary>
        /// <param name="name">Name of the scope searched.</param>
        /// <returns>A non-null container entry of scopes matching the given name.</returns>
        [NotNull]
        public Container<ScopeSymbol>.Entry LookupScope([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);

            if (_allScopes.TryGetValue(name, out var result))
                return result;

            return new Container<ScopeSymbol>.Entry(name);
        }

        /// <summary>
        /// Searches for types of this RootSymbolTable having the given name.
        /// </summary>
        /// <param name="name">Name of the type searched.</param>
        /// <returns>A non-null container entry of types matching the given name.</returns>
        [NotNull]
        public Container<TypedefSymbol>.Entry LookupType([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);

            if (_allTypes.TryGetValue(name, out var result))
                return result;

            return new Container<TypedefSymbol>.Entry(name);
        }

        /// <summary>
        /// Resolve any ScopeSymbol. Namespace, program and function are abstract scopes.
        /// </summary>
        /// <param name="path">The Abstract scope path</param>
        /// <param name="kinds">All kinds of scope to be resolved.</param>
        /// <returns>A Scope instance of matches</returns>
        private Container<TScope>.Entry ResolveScope<TScope>(string[] path, params Symbol.Kinds[] kinds)
            where TScope : ScopeSymbol
        {
            var candidates = ResolveSymbol<ScopeSymbol>(path, LookupScope);
            if (candidates == null)
                return null;

            Container<TScope>.Entry results = new Container<TScope>.Entry(candidates.Name);
            kinds = kinds == null || kinds.Length == 0 ? _AllScopeKinds : kinds;
            foreach (var candidate in candidates)
            {
                if (kinds.Contains(candidate.Kind))
                    results.Add((TScope)candidate);
            }

            return results;
        }

        /// <summary>
        /// Resolve a namespace path
        /// </summary>
        /// <param name="path">The namespace path</param>
        /// <returns></returns>
        public Container<NamespaceSymbol>.Entry ResolveNamespace(string[] path)
        {
            return ResolveScope<NamespaceSymbol>(path, Kinds.Namespace);
        }

        /// <summary>
        /// Resolve a Program path
        /// </summary>
        /// <param name="path">The Program's path'</param>
        /// <returns>The set of matching results</returns>
        public Container<ProgramSymbol>.Entry ResolveProgram(string[] path)
        {
            return ResolveScope<ProgramSymbol>(path, Kinds.Program);
        }

        /// <summary>
        /// Resolve a Function path
        /// </summary>
        /// <param name="path">The function's path'</param>
        /// <returns></returns>
        public Container<FunctionSymbol>.Entry ResolveFunction(string[] path)
        {
            return ResolveScope<FunctionSymbol>(path, Kinds.Function);
        }

        /// <summary>
        /// Resolve a Program or a Function path
        /// </summary>
        /// <param name="path">The program's' or function's path'</param>
        /// <returns>The set of matching results</returns>
        public Container<ProgramSymbol>.Entry ResolveProgramOrFunction(string[] path)
        {
            return ResolveScope<ProgramSymbol>(path, Kinds.Program, Kinds.Function);
        }

        /// <summary>
        /// Resolve a Type
        /// </summary>
        /// <param name="path">Type's path'</param>
        /// <returns>The set of matching results</returns>
        public Container<TypedefSymbol>.Entry ResolveType(string[] path)
        {
            return ResolveSymbol<TypedefSymbol>(path, LookupType);
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// The Root Symbol Table is a special Namespace
    /// </summary>
    public class RootSymbolTable : NamespaceSymbol
    {
        /// <summary>
        /// All Kinds of scope that contains symbols (i.e. inheritors of AbstractScope except RootSymbolTable).
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
        /// This is in fact the entire domain of variable within this Root Symbol Table.
        /// </summary>
        private IList<VariableSymbol> Universe { get; }

        /// <summary>
        /// The Domain of all namespaces, programs and functions.
        /// </summary>
        private Domain<AbstractScope> ScopeDomain { get; }

        /// <summary>
        /// The Domain of all types.
        /// </summary>
        private Domain<TypedefSymbol> TypeDomain { get; }

        private readonly List<VariableTypeSymbol> _variablesThatNeedTypeLinking;

        /// <summary>
        /// Empty Constructor.
        /// </summary>
        public RootSymbolTable()
            : base(string.Intern("<<Root>>"))
        {
            base.Kind = Kinds.Root;

            _variableSymbolIndex = 0;
            _globalIndexPool = new Stack<int>();

            Universe = new List<VariableSymbol>();
            ScopeDomain = new Domain<AbstractScope>();
            TypeDomain = new Domain<TypedefSymbol>();
            _variablesThatNeedTypeLinking = new List<VariableTypeSymbol>();

            //Register BottomVariable
            AddToUniverse(BottomVariable);

            //Load Builtin symbols
            SymbolTableBuilder.AddBuiltinSymbol(this);
        }

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
        /// Add the given VariableSymbol instance in this Root Symbol Table universe
        /// </summary>
        /// <param name="varSym">The Variable Symbol to be added</param>
        /// <returns>The given VariableSymbol instance.</returns>
        internal VariableSymbol AddToUniverse(VariableSymbol varSym)
        {
            System.Diagnostics.Debug.Assert(varSym != null);
            System.Diagnostics.Debug.Assert(varSym.GlobalIndex == 0);

            varSym.GlobalIndex = NextVariableSymbolIndex();
            Universe.Add(varSym);
            return varSym;
        }

        /// <summary>
        /// Remove from the universe the given variable symbol.
        /// </summary>
        /// <param name="varSym">The variable symbol to be removed</param>
        internal void RemoveFromUniverse(VariableSymbol varSym)
        {
            System.Diagnostics.Debug.Assert(varSym != null);
            System.Diagnostics.Debug.Assert(varSym.GlobalIndex != 0);

            if (varSym.GlobalIndex != 0)
            {
                Universe[varSym.GlobalIndex] = null;
                _globalIndexPool.Push(varSym.GlobalIndex);
                varSym.GlobalIndex = 0;
            }
        }

        /// <summary>
        /// Add the given AbstractScope instance the domain
        /// </summary>
        /// <param name="absScope">Abstract Scope to be added</param>
        public override void AddToDomain(AbstractScope absScope)
        {
            System.Diagnostics.Debug.Assert(absScope != null);
            ScopeDomain.Add(absScope);
            if (ProgramAdded != null && absScope.Kind == Kinds.Program)
                ProgramAdded(this, new SymbolEventArgs(absScope));
        }

        /// <summary>
        /// Remove the given scope from the domain.
        /// </summary>
        /// <param name="absScope">The Scope to be removed</param>
        public override void RemoveFromDomain(AbstractScope absScope)
        {
            ScopeDomain.Remove(absScope);

            absScope.FreeDomain();

            //Remove all Types
            var types = absScope.Types;
            if (types != null)
            {
                foreach (var t in types)
                {
                    RemoveFromDomain(t);
                }
            }

            //Remove all programs
            var programs = absScope.Programs;
            if (programs != null)
            {
                foreach (var p in programs)
                {
                    RemoveFromDomain(p);
                }
            }

            //Remove all functions
            var functions = absScope.Functions;
            if (functions != null)
            {
                foreach (var p in functions)
                {
                    RemoveFromDomain(p);
                }
            }

            //Special case Namespace
            if (absScope.Kind == Kinds.Namespace)
            {
                NamespaceSymbol ns = (NamespaceSymbol)absScope;
                var nss = ns.Namespaces;
                if (nss != null)
                {
                    foreach (var n in nss)
                    {
                        RemoveFromDomain(n);
                    }
                }
            }
        }

        /// <summary>
        /// Add the given Type instance the domain
        /// </summary>
        /// <param name="type">The type to add to be added</param>
        public override void AddToDomain(TypedefSymbol type)
        {
            System.Diagnostics.Debug.Assert(type != null);
            TypeDomain.Add(type);
        }

        /// <summary>
        /// Remove the given type from the domain.
        /// </summary>
        /// <param name="type">The type to be removed</param>
        public override void RemoveFromDomain(TypedefSymbol type)
        {
            System.Diagnostics.Debug.Assert(type != null);
            TypeDomain.Remove(type);
        }

        /// <summary>
        /// Performs a search in the universe of variables of this RootSymbolTable
        /// using a name.
        /// </summary>
        /// <param name="name">Name of the variable searched.</param>
        /// <returns>A non-null domain entry of variables matching the given name.</returns>
        [NotNull]
        public Domain<VariableSymbol>.Entry LookupVariable([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);

            var results = new Domain<VariableSymbol>.Entry(name);
            foreach (var variableSymbol in Universe.Where(v => string.Equals(v.Name, name, StringComparison.OrdinalIgnoreCase)))
            {
                results.Add(variableSymbol);
            }

            return results;
        }

        /// <summary>
        /// Searches for scopes of this RootSymbolTable having the given name.
        /// </summary>
        /// <param name="name">Name of the scope searched.</param>
        /// <returns>A non-null domain entry of scopes matching the given name.</returns>
        [NotNull]
        public Domain<AbstractScope>.Entry LookupScope([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);

            if (ScopeDomain.TryGetValue(name, out var result))
                return result;

            return new Domain<AbstractScope>.Entry(name);
        }

        /// <summary>
        /// Searches for types of this RootSymbolTable having the given name.
        /// </summary>
        /// <param name="name">Name of the type searched.</param>
        /// <returns>A non-null domain entry of types matching the given name.</returns>
        [NotNull]
        public Domain<TypedefSymbol>.Entry LookupType([NotNull] string name)
        {
            System.Diagnostics.Debug.Assert(name != null);

            if (TypeDomain.TryGetValue(name, out var result))
                return result;

            return new Domain<TypedefSymbol>.Entry(name);
        }

        /// <summary>
        /// Resolve any AbstractScope. Namespace, program and function are abstract scopes.
        /// </summary>
        /// <param name="path">The Abstract scope path</param>
        /// <param name="kinds">All kinds of scope to be resolved.</param>
        /// <returns>A Scope instance of matches</returns>
        private Domain<TScope>.Entry ResolveScope<TScope>(string[] path, params Symbol.Kinds[] kinds)
            where TScope : AbstractScope
        {
            if (path == null || path.Length == 0 || path[0] == null)
                return null;

            string name = path[0];
            Domain<TScope>.Entry results = new Domain<TScope>.Entry(name);
            var candidates = LookupScope(name);
            kinds = kinds == null || kinds.Length == 0 ? _AllScopeKinds : kinds;
            foreach (var candidate in candidates)
            {
                if (kinds.Contains(candidate.Kind) && candidate.IsMatchingPath(path))
                    results.Add((TScope)candidate);
            }

            return results;
        }

        /// <summary>
        /// Resolve a namespace path
        /// </summary>
        /// <param name="path">The Namespace's path'</param>
        /// <returns></returns>
        public Domain<NamespaceSymbol>.Entry ResolveNamespace(string[] path)
        {
            return ResolveScope<NamespaceSymbol>(path, Kinds.Namespace);
        }

        /// <summary>
        /// Resolve a Program path
        /// </summary>
        /// <param name="path">The Program's path'</param>
        /// <returns>The set of matching results</returns>
        public Domain<ProgramSymbol>.Entry ResolveProgram(string[] path)
        {
            return ResolveScope<ProgramSymbol>(path, Kinds.Program);
        }

        /// <summary>
        /// Resolve a Function path
        /// </summary>
        /// <param name="path">The function's path'</param>
        /// <returns></returns>
        public Domain<FunctionSymbol>.Entry ResolveFunction(string[] path)
        {
            return ResolveScope<FunctionSymbol>(path, Kinds.Function);
        }

        /// <summary>
        /// Resolve a Program or a Function path
        /// </summary>
        /// <param name="path">The program's' or function's path'</param>
        /// <returns>The set of matching results</returns>
        public Domain<ProgramSymbol>.Entry ResolveProgramOrFunction(string[] path)
        {
            return ResolveScope<ProgramSymbol>(path, Kinds.Program, Kinds.Function);
        }

        /// <summary>
        /// Resolve a Type
        /// </summary>
        /// <param name="path">Type's path'</param>
        /// <param name="bIncludeUndefined">True if undefined type must also be included, false otherwise</param>
        /// <returns>The set of matching results</returns>
        public Domain<TypedefSymbol>.Entry ResolveQualifiedType(string[] path, bool bIncludeUndefined = false)
        {
            if (path == null || path.Length == 0 || path[0] == null)
                return null;

            string name = path[0];
            var results = new Domain<TypedefSymbol>.Entry(name);
            var candidates = LookupType(name);
            foreach (var candidate in candidates)
            {
                //Only selected whose Type is defined.
                if ((bIncludeUndefined || candidate.Type != null) && candidate.IsMatchingPath(path))
                    results.Add(candidate);
            }

            return results;
        }

        public void RegisterForTypeLinking(VariableTypeSymbol variableTypeSymbol)
        {
            _variablesThatNeedTypeLinking.Add(variableTypeSymbol);
        }

        public void UpdateTypeLinks()
        {
            var variablesThatNeedTypeLinking = _variablesThatNeedTypeLinking.ToArray();
            foreach (var variableThatNeedTypeLinking in variablesThatNeedTypeLinking)
            {
                ProgramSymbol program = (ProgramSymbol) variableThatNeedTypeLinking.NearestKind(Symbol.Kinds.Program, Symbol.Kinds.Function);
                Domain<TypedefSymbol>.Entry entry = program.ResolveType(this, variableThatNeedTypeLinking.TypePaths);
                if (entry != null && entry.Count == 1)
                {
                    //Successfully resolved Typedef symbol
                    variableThatNeedTypeLinking.Typedef = entry.Symbol;
                    _variablesThatNeedTypeLinking.Remove(variableThatNeedTypeLinking);

                    //--------------------------------------------------------------------------------------------
                    //We don't check type accessibility here. I think that the semantic analyzer should do that.
                    //This can be achieved by the following call:
                    //program.IsTypeAccessible(entry.Symbol);
                    //--------------------------------------------------------------------------------------------
                }
            }
        }
    }
}

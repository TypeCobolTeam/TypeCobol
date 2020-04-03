using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Represents any symbol that contain other symbols (i.e. ProgramSymbol or NamespaceSymbol)
    /// </summary>
    public abstract class ScopeSymbol : Symbol, IScope
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        protected ScopeSymbol(string name, Kinds kind)
            : base(name, kind)
        {
        }

        public virtual Domain<TypedefSymbol> Types
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> FileData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> GlobalStorageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> WorkingStorageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> LocalStorageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<VariableSymbol> LinkageData
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<SectionSymbol> Sections
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<ParagraphSymbol> Paragraphs
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<FunctionSymbol> Functions
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Domain<ProgramSymbol> Programs
        {
            get { return null; }
            protected set { ; }
        }

        /// <summary>
        /// Resolve a Symbol.
        /// For a single symbol's name, the algorithm tries to resolve types starting from the local scope to the Top scope.
        /// For a qualified symbol, the algorithm tries to filter symbols whose qualified names strictly match the path.
        /// </summary>
        /// <param name="path">The type's path'</param>
        /// <param name="topScope">The top scope of the research</param>
        /// <param name="lookupSymbol">Delegate to lookup symbol inside a RootSymbolTable</param>
        /// <returns>The Set of resolve symbols</returns>
        protected Container<TS>.Entry ResolveSymbol<TS>(string[] path, ScopeSymbol topScope, Func<string, Container<TS>.Entry> lookupSymbol)
            where TS : Symbol
        {
            if (path == null || path.Length == 0 || path[0] == null)
                return null;

            string name = path[0];
            Container<TS>.Entry results = new Container<TS>.Entry(name);
            var candidates = lookupSymbol(name);
            if (candidates.Count == 0)
                return results;

            if (path.Length == 1)
            {
                bool bLocal = topScope != this;
                Container<TS>.Entry localResults = bLocal ? new Container<TS>.Entry(name) : null;
                Container<TS>.Entry topResults = results;
                string[] topPath = new string[] { name, topScope.Name };
                string[] localPath = bLocal ? new string[] { name, this.Name, topScope.Name } : null;
                foreach (var candidate in candidates)
                {
                    if (bLocal && candidate.IsMatchingPath(localPath))
                    {
                        localResults.Add(candidate);
                    }
                    else if (candidate.IsMatchingPath(topPath))
                    {
                        topResults.Add(candidate);
                    }
                }
                if (bLocal && localResults.Count != 0)
                    return localResults;
                if (topResults.Count != 0)
                    return topResults;
                return candidates;
            }
            foreach (var candidate in candidates)
            {
                if (candidate.IsStrictlyMatchingPath(path))
                {
                    results.Add(candidate);
                }
            }
            return results.Count == 0 ? candidates : results;
        }

        /// <summary>
        /// Resolve all types accessible from this scope by its path.
        /// </summary>
        /// <param name="root">The Root Symbol Table</param>
        /// <param name="path">The type's path</param>
        /// <returns>The set of types that match</returns>
        public abstract Container<TypedefSymbol>.Entry ResolveType(RootSymbolTable root, string[] path);

        /// <summary>
        /// Resolve all scopes accessible from this scope by its path.
        /// A Scope can be a namespace, a program or a function.
        /// </summary>
        /// <param name="root">The Root Symbol Table</param>
        /// <param name="path">The function's path</param>
        /// <returns>The set of scopes that match</returns>
        public abstract Container<ScopeSymbol>.Entry ResolveScope(RootSymbolTable root, string[] path);

        /// <summary>
        /// Looks for a symbol designated by the given path.
        /// Searches from this current ScopeSymbol up to rootScope.
        /// </summary>
        /// <typeparam name="TSymbol">Type of symbols searched.</typeparam>
        /// <param name="path">Looking path à la COBOL85 --> in Reverse order</param>
        /// <param name="rootScope">Top Scope allowed for searching.</param>
        /// <param name="domainSelector">Delegate to select the correct domain of symbols (i.e. Types, Functions, etc)</param>
        /// <returns>The symbol entry if found, null otherwise.</returns>
        private Container<TSymbol>.Entry ReverseResolveSymbol<TSymbol>(string[] path, ScopeSymbol rootScope, Func<ScopeSymbol, Domain<TSymbol>> domainSelector)
            where TSymbol : Symbol
        {
            System.Diagnostics.Debug.Assert(path != null);
            System.Diagnostics.Debug.Assert(path.Length > 0);
            System.Diagnostics.Debug.Assert(path[0] != null);
            System.Diagnostics.Debug.Assert(rootScope != null);
            System.Diagnostics.Debug.Assert(domainSelector != null);

            ScopeSymbol currentScope = this;
            ScopeSymbol stopScope = rootScope;

            //Main loop : we iterate over the part of the path
            for (int i = path.Length - 1; i >= 0; i--)
            {
                switch (i)
                {
                    case 0: //We must look for the target Symbol
                        return Lookup(path[i], domainSelector);

                    case 1: //We must look for a Program declaring the target Symbol
                        var programSymbolEntry = Lookup(path[i], s => s.Programs);
                        if (programSymbolEntry != null)
                        {
                            System.Diagnostics.Debug.Assert(programSymbolEntry.Count == 1);
                            //Continue searching for target symbol inside found Program
                            currentScope = programSymbolEntry.Symbol;
                            stopScope = currentScope;
                        }
                        else
                        {
                            //Could not resolve program, abort search.
                            return null;
                        }
                        break;

                    default: //We are looking for a Namepace
                        //TODO
                        break;
                }
            }

            return null;

            //Secondary loop : we iterate over scopes until we either find the symbol or reach stopScope.
            Container<T>.Entry Lookup<T>(string name, Func<ScopeSymbol, Domain<T>> getDomain)
                where T : Symbol
            {
                Container<T>.Entry result = null;
                while (currentScope != null)
                {
                    //Search current Scope.
                    var domain = getDomain(currentScope);
                    if (domain != null)
                    {
                        result = domain.Lookup(name);
                        if (result != null)
                        {
                            stopScope = currentScope;
                            break;
                        }
                    }

                    //The current scope lookup failed, move up to parent Scope (if possible).
                    if (currentScope.Owner != null && currentScope != stopScope && currentScope.Owner.HasScope)
                    {
                        currentScope = currentScope.Owner as ScopeSymbol;
                    }
                    else
                    {
                        break;
                    }
                }

                return result;
            }
        }

        /// <summary>
        /// Resolve a TypeDefinition from this current Scope.
        /// </summary>
        /// <param name="rootScope">The top rootScope</param>
        /// <param name="path">Looking path à la COBOL85 --> in Reverse order</param>
        /// <returns>The TypedefSymbol if found, null otherwise.</returns>
        public Container<TypedefSymbol>.Entry ReverseResolveType(ScopeSymbol rootScope, string[] path)
        {
            return ReverseResolveSymbol(path, rootScope, s => s.Types);
        }

        /// <summary>
        /// Resolve a Function symbol from this current Scope.
        /// </summary>
        /// <param name="rootScope">The top rootScope</param>
        /// <param name="path">Looking path à la COBOL85 --> in Reverse order</param>
        /// <returns>The FunctionSymbol instance if found, null otherwise.</returns>
        public Container<FunctionSymbol>.Entry ReverseResolveFunction(ScopeSymbol rootScope, string[] path)
        {
            return ReverseResolveSymbol(path, rootScope, s => s.Functions);
        }

        /// <summary>
        /// Compute the path represented by a Symbol Reference
        /// </summary>
        /// <param name="symRef">The Symbol Reference instance</param>
        /// <returns>The corresponding Path in the COBOL IN|OF ORDER. The paths are return ed in lower cases</returns>
        public static string[] SymbolReferenceToPath(SymbolReference datSymRef)
        {
            string[] paths = null;
            IList<SymbolReference> refs = null;

            if (datSymRef.IsQualifiedReference)
            {//Path in reverse order DVZF0OS3::EventList --> {EventList, DVZF0OS3}
                QualifiedSymbolReference qualifiedSymbolReference = datSymRef as QualifiedSymbolReference;
                refs = qualifiedSymbolReference.AsList();
            }
            else
            {
                refs = new List<SymbolReference>() { datSymRef };
            }

            paths = new string[refs.Count];
            for (int i = 0; i < refs.Count; i++)
            {
                paths[i] = refs[i].Name;
            }

            return paths;
        }

        /// <summary>
        /// Discard all symbols and types currently held by this Scope from the RootSymbolTable.
        /// The symbols and types stay in this ScopeSymbol and their respective owners do not change
        /// but they are not known anymore by the Root Table.
        /// </summary>
        internal virtual void DiscardSymbolsFromRoot()
        { }
    }
}

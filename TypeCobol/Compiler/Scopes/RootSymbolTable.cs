using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        /// This is the first variable of the universe that can be assimilated to the 0 or null variable.
        /// </summary>
        public VariableSymbol BottomVariable { get; private set; }

        /// <summary>
        /// The count of all variable created in this RootSymbolTable
        /// </summary>
        private uint _variableSymbolCounter = 0;

        /// <summary>
        /// Empty Constructor.
        /// </summary>
        public RootSymbolTable() : base("<<Root>>")
        {
            base.Kind = Kinds.Root;
            Universe = new List<VariableSymbol>();
            ScopeDomain = new Dictionary<string, Scope<AbstractScope>.MultiSymbols>();
            BottomVariable = new VariableSymbol("<<BottomVariable>>");
            AddToUniverse(BottomVariable);
        }

        /// <summary>
        /// Full qualified name of this Symbol à la TypeCobol using "::"
        /// </summary>
        public override String FullName => "";

        /// <summary>
        /// Full qualified name of this Symbol à la COBOL85 using OF
        /// </summary>
        public override String FullOfName => "";

        /// <summary>
        /// Full dotted qualified name
        /// </summary>
        public override String FullDotName => "";

        /// <summary>
        /// Full typed dotted qualified name
        /// </summary>
        public override String FullTypedDotName => "";

        public override String TypedName => "";


        /// <summary>
        /// The Count of Variable Symbol created
        /// </summary>
        public uint VariableSymbolCount => _variableSymbolCounter;

        /// <summary>
        /// Get the Next VariableSymbol Context.
        /// </summary>
        /// <returns></returns>
        private uint NextVariableSymbolIndex()
        {
            return _variableSymbolCounter++;
        }

        /// <summary>
        /// Add the given VariableSymbol instance in this Root Symbol Table universe
        /// </summary>
        /// <param name="varSym">The Variable Symbol to be added</param>
        /// <returns>The given VariableSymbol instance.</returns>
        public VariableSymbol AddToUniverse(VariableSymbol varSym)
        {
            System.Diagnostics.Debug.Assert(varSym != null);
            System.Diagnostics.Debug.Assert(varSym.GlobalIndex == 0);
            lock (Universe)
            {
                varSym.GlobalIndex = NextVariableSymbolIndex();
                Universe.Add(varSym);
            }

            return varSym;
        }

        /// <summary>
        /// All Ordered Symbol that can be reached from this Root Symbol Table.
        /// This is in fact the entire domain of variable within this Global Symbol Table.
        /// </summary>
        public IList<VariableSymbol> Universe { get; internal set; }

        /// <summary>
        /// The Domain of all namespaces, programs and functions.
        /// </summary>
        internal Dictionary<string, Scope<AbstractScope>.MultiSymbols> ScopeDomain { get; private set; }

        /// <summary>
        /// Add the given AbstractScope instance the domain
        /// </summary>
        /// <param name="absScope">Abstract Scope</param>
        /// <returns>The given VariableSymbol instance.</returns>
        public void AddToDomain(AbstractScope absScope)
        {
            System.Diagnostics.Debug.Assert(absScope != null);
            lock (ScopeDomain)
            {
                string name = absScope.Name;
                ScopeDomain.TryGetValue(name, out var value);
                if (value == null)
                    ScopeDomain[name] = new Scope<AbstractScope>.MultiSymbols(absScope);
                else
                    value.Add(absScope);
            }
        }

        /// <summary>
        /// Remove the given scope from the domain.
        /// </summary>
        /// <param name="absScope">The Scope to be removed</param>
        public void RemoveFromDomain(AbstractScope absScope)
        {
            string name = absScope.Name;
            ScopeDomain.TryGetValue(name, out var value);
            if (value != null)
            {
                value.Remove(absScope);

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
                    NamespaceSymbol ns = (NamespaceSymbol) absScope;
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
        }

        /// <summary>
        /// All Kinds of scope that.
        /// </summary>
        private static readonly Symbol.Kinds[] AllScopeKinds = new Kinds[]
            {Kinds.Namespace, Kinds.Program, Kinds.Function};

        /// <summary>
        /// Resolve any AbstractScope. Namespace, program and function are abstract scopes.
        /// </summary>
        /// <param name="path">The Abstract scope path</param>
        /// <param name="kinds">All kinds of scope to be resolved.</param>
        /// <returns>A Scope instance of matches</returns>
        public Scope<TS>.MultiSymbols ResolveScope<TS>(string[] path, params Symbol.Kinds[] kinds)
            where TS : AbstractScope
        {
            Scope<TS>.MultiSymbols results = new Scope<TS>.MultiSymbols();
            if (path == null || path.Length == 0)
                return results;

            string name = path[0];
            this.ScopeDomain.TryGetValue(name, out var candidates);
            if (candidates != null)
            {
                kinds = kinds == null || kinds.Length == 0 ? AllScopeKinds : kinds;
                foreach (var candidate in candidates)
                {
                    if (kinds.Contains(candidate.Kind) && candidate.IsMatchingPath(path))
                        results.Add((TS) candidate);
                }
            }

            return results;
        }

        /// <summary>
        /// Resolve a namespace path
        /// </summary>
        /// <param name="path">The Namespace's path'</param>
        /// <returns></returns>
        public Scope<NamespaceSymbol>.MultiSymbols ResolveNamespace(string[] path)
        {
            return ResolveScope<NamespaceSymbol>(path, Kinds.Namespace);
        }

        /// <summary>
        /// Resolve a Program path
        /// </summary>
        /// <param name="path">The Program's path'</param>
        /// <returns></returns>
        public Scope<ProgramSymbol>.MultiSymbols ResolveProgram(string[] path)
        {
            return ResolveScope<ProgramSymbol>(path, Kinds.Program);
        }

        /// <summary>
        /// Resolve a Function path
        /// </summary>
        /// <param name="path">The function's path'</param>
        /// <returns></returns>
        public Scope<FunctionSymbol>.MultiSymbols ResolveFunction(string[] path)
        {
            return ResolveScope<FunctionSymbol>(path, Kinds.Function);
        }

        /// <summary>
        /// Resolve a Program or a Function path
        /// </summary>
        /// <param name="path">The program's' or function's path'</param>
        /// <returns></returns>
        public Scope<ProgramSymbol>.MultiSymbols ResolveProgramOrFunction(string[] path)
        {
            return ResolveScope<ProgramSymbol>(path, Kinds.Program, Kinds.Function);
        }

        /// <summary>
        /// Resolve a Type, if the type path is not qualified then the type is search into Intrinsic types
        /// otherwise the qualification is resolved to a Program or a Function and the type name in
        /// search into each program of function.
        /// </summary>
        /// <param name="path">The program's' or function's path'</param>
        /// <returns>null if the</returns>
        public Scope<TypedefSymbol>.MultiSymbols ResolveQualifiedType(string[] path)
        {
            Scope<TypedefSymbol>.MultiSymbols results = new Scope<TypedefSymbol>.MultiSymbols();
            if (path == null || path.Length == 0)
                return results;
            string[] name = { path[0] };
            switch (path.Length)
            {
                case 1: //Only look into intrinsics.
                {
                    var types = this.ReverseResolveType(this, name, false);
                    if (types != null && types.Count >= 0)
                    {
                        foreach (var t in types)
                        {
                            results.Add(t);
                        }
                    }
                }
                    break;
                default:
                {
                    //First resolve the qualified scope in to Programs only program contains Type.
                    string[] nsPath = new string[path.Length - 1];
                    Array.Copy(path, 1, nsPath, 0, nsPath.Length);
                    Scope<ProgramSymbol>.MultiSymbols allPrgs = ResolveProgramOrFunction(nsPath);
                    foreach (ProgramSymbol p in allPrgs)
                    {
                        var types = p.ReverseResolveType(this, name, false);
                        if (types != null && types.Count >= 0)
                        {
                            foreach (var t in types)
                            {
                                results.Add(t);
                            }

                            ;
                        }
                    }
                }
                    break;
            }

            return results;
        }
    }
}

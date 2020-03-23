using System;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Symbol that represents a Namespace. A namespace can only contains 
    /// programs or namespaces.
    /// </summary>
    public class NamespaceSymbol : ScopeSymbol
    {
        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name"></param>
        public NamespaceSymbol(string name)
            : base(name, Kinds.Namespace)
        {
            Programs = new Domain< ProgramSymbol >(this);
            Namespaces = new Domain<NamespaceSymbol>(this);
        }

        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="name"></param>
        /// <param name="from"></param>
        public NamespaceSymbol(string name, NamespaceSymbol from) : this(name)
        {
            foreach (var p in from.Programs)
            {
                Programs.Enter(p);
            }
            foreach (var n in from.Namespaces)
            {
                Namespaces.Enter(n);
            }
        }

        /// <summary>
        /// Enter a Program in this namespace
        /// </summary>
        /// <param name="name">Program's name</param>
        /// <returns>The ProgramSymbol</returns>
        public ProgramSymbol EnterProgram(string name)
        {
            Container<ProgramSymbol>.Entry entry = Programs.Lookup(name);
            if (entry == null) 
            {
                ProgramSymbol prgSym = new ProgramSymbol(name);
                entry = Programs.Enter(prgSym);
            }
            //Set the owner
            entry.Symbol.Owner = this;
            //Add it to the root symbol table.
            Symbol root = TopParent(Kinds.Root);
            ((RootSymbolTable)root)?.Add(entry.Symbol);
            return entry.Symbol;
        }

        /// <summary>
        /// Remove a program;
        /// </summary>
        /// <param name="prgSym">The program to be removed</param>
        public void RemoveProgram(ProgramSymbol prgSym)
        {
            if (prgSym != null)
            {
                Programs.Delete(prgSym);
                //Remove it from the root symbol table.
                Symbol root = TopParent(Kinds.Root);
                ((RootSymbolTable)root)?.Remove(prgSym);
                prgSym.Owner = null;
            }
        }

        /// <summary>
        /// All programs declared in this namespace.
        /// </summary>
        public override Domain<ProgramSymbol> Programs
        {
            get;
            protected set;
        }

        /// <summary>
        /// All namespaces declared in this namespace.
        /// </summary>
        public Domain<NamespaceSymbol> Namespaces
        {
            get;
            protected set;
        }

        protected Container<TSymbol>.Entry ResolveSymbol<TSymbol>(string[] path, Func<string, Container<TSymbol>.Entry> lookupSymbol)
            where TSymbol : Symbol
        {
            if (path == null || path.Length == 0 || path[0] == null)
                return null;

            var name = path[0];
            var results = new Container<TSymbol>.Entry(name);
            foreach (var candidate in lookupSymbol(name))
            {
                if (candidate.IsMatchingPath(path))
                {
                    results.Add(candidate);
                }
            }

            return results;
        }

        public override Container<TypedefSymbol>.Entry ResolveType(RootSymbolTable root, string[] path)
        {
            throw new InvalidOperationException("Namespace symbol does not contain any type.");
        }

        public override Container<ScopeSymbol>.Entry ResolveScope(RootSymbolTable root, string[] path)
        {
            return ResolveSymbol<ScopeSymbol>(path, root.LookupScope);
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitNamespaceSymbol(this, arg); }
    }
}

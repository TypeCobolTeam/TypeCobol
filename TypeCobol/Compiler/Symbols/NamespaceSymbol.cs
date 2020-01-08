using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Symbol that represents a Namespace. A namespace can only contains 
    /// programs or namespaces.
    /// </summary>
    public class NamespaceSymbol : AbstractScope
    {
        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name"></param>
        public NamespaceSymbol(String name)
            : base(name, Kinds.Namespace)
        {
            Types = new Scope<TypedefSymbol>(this);
            Programs = new Scope < ProgramSymbol >(this);
            Namespaces = new Scope<NamespaceSymbol>(this);
        }

        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="name"></param>
        /// <param name="from"></param>
        public NamespaceSymbol(String name, NamespaceSymbol from) : this(name)
        {
            foreach (var t in from.Types)
            {
                Types.Enter(t);
            }
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
        public ProgramSymbol EnterProgram(String name)
        {
            Domain<ProgramSymbol>.Entry entry = Programs.Lookup(name);
            if (entry == null) 
            {
                ProgramSymbol prgSym = new ProgramSymbol(name);
                entry = Programs.Enter(prgSym);
            }
            //Set the owner
            entry.Symbol.Owner = this;
            //Add it to the all scope domain
            Symbol root = TopParent(Kinds.Root);
            ((RootSymbolTable)root)?.AddToDomain(entry.Symbol);
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
                //Remove it from the all scope domain
                Symbol root = TopParent(Kinds.Root);
                ((RootSymbolTable)root)?.RemoveFromDomain(prgSym);
                prgSym.Owner = null;
            }
        }

        /// <summary>
        /// All Types declared in this namespace
        /// </summary>
        public override Scope<TypedefSymbol> Types
        {
            get;
            protected set;
        }

        /// <summary>
        /// All programs declared in this namespace.
        /// </summary>
        public override Scope<ProgramSymbol> Programs
        {
            get;
            protected set;
        }

        public override Domain<TypedefSymbol>.Entry ResolveAccessibleType(RootSymbolTable root, string[] path)
        {
            throw new NotImplementedException();
        }

        public override Domain<TypedefSymbol>.Entry ResolveType(RootSymbolTable root, string[] path)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// All namespaces declared in this namespace.
        /// </summary>
        public Scope<NamespaceSymbol> Namespaces
        {
            get;
            protected set;
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitNamespaceSymbol(this, arg); }

        public override Domain<AbstractScope>.Entry ResolveScope(RootSymbolTable root, string[] path)
        {
            throw new NotImplementedException();
        }
    }
}

using System.IO;
using JetBrains.Annotations;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Base class for ProgramSymbol and FunctionSymbol.
    /// A ScopeSymbol contains other symbols and has a ScopeType.
    /// </summary>
    public abstract class ScopeSymbol : Symbol
    {
        /// <summary>
        /// Helper method to dump a domain into a TextWriter.
        /// </summary>
        /// <typeparam name="TSymbol">Type of symbols in the given domain.</typeparam>
        /// <param name="output">TextWriter instance to write to.</param>
        /// <param name="indentLevel">Initial indentation level.</param>
        /// <param name="domainDisplayName">Name of the given domain.</param>
        /// <param name="domain">Domain instance to be dumped.</param>
        protected static void DumpDomain<TSymbol>(TextWriter output, int indentLevel, string domainDisplayName, Domain<TSymbol> domain)
            where TSymbol : Symbol
        {
            if (domain != null && domain.Count > 0)
            {
                string indent = new string(' ', 2 * indentLevel);
                output.Write(indent);
                output.WriteLine($"{domainDisplayName}:");
                var level = indentLevel + 1;
                foreach (var symbol in domain)
                {
                    symbol.Dump(output, level);
                }
            }
        }

        /// <summary>
        /// All variables defined in this scope.
        /// </summary>
        private readonly Container<VariableSymbol> _variables;

        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name">Scope's name.</param>
        /// <param name="kind">Scope's kind.</param>
        protected ScopeSymbol(string name, Kinds kind)
            : base(name, kind)
        {
            _variables = new Container<VariableSymbol>();
            Types = new Domain<TypedefSymbol>(this);
            WorkingStorageData = new Domain<VariableSymbol>(this);
            LocalStorageData = new Domain<VariableSymbol>(this);
            LinkageData = new Domain<VariableSymbol>(this);
            Sections = new Domain<SectionSymbol>(this);
            Paragraphs = new Domain<ParagraphSymbol>(this);
        }

        new public ScopeType Type
        {
            get
            {
                System.Diagnostics.Debug.Assert(base.Type is ScopeType);
                return (ScopeType) base.Type;
            }
            set => base.Type = value;
        }

        /// <summary>
        /// Types defined in this Scope.
        /// </summary>
        public Domain<TypedefSymbol> Types { get; }

        /// <summary>
        /// Working storage section variables.
        /// </summary>
        public Domain<VariableSymbol> WorkingStorageData { get; }

        /// <summary>
        /// Local storage section variables.
        /// </summary>
        public Domain<VariableSymbol> LocalStorageData { get; }

        /// <summary>
        /// Linkage section variables.
        /// </summary>
        public Domain<VariableSymbol> LinkageData { get; }

        /// <summary>
        /// Sections defined in this Scope.
        /// </summary>
        public Domain<SectionSymbol> Sections { get; }

        /// <summary>
        /// Paragraphs defined in this Scope.
        /// </summary>
        public Domain<ParagraphSymbol> Paragraphs { get; }

        /// <summary>
        /// Get the variable visibility mask.
        /// </summary>
        protected abstract Flags VariableVisibilityMask { get; }

        /// <summary>
        /// Get the type visibility mask.
        /// </summary>
        protected abstract Flags TypeVisibilityMask { get; }

        /// <summary>
        /// Get the function visibility mask.
        /// </summary>
        protected abstract Flags FunctionVisibilityMask { get; }

        /// <summary>
        /// Determines if a Symbol is accessible using an accessibility mask.
        /// </summary>
        /// <param name="sym">The symbol to be checked</param>
        /// <param name="mask">The accessibility mask</param>
        /// <returns>true if the symbol is accessible, false otherwise.</returns>
        private bool IsSymbolAccessible(Symbol sym, Flags mask)
        {
            System.Diagnostics.Debug.Assert(sym != null);
            if (sym.HasFlag(Flags.BuiltinSymbol))
                return true; //Builtin symbols are always accessible.

            Symbol symTopPrg = sym.TopParent(Kinds.Program);
            System.Diagnostics.Debug.Assert(symTopPrg != null);
            Symbol myTopPrg = TopParent(Kinds.Program);
            System.Diagnostics.Debug.Assert(myTopPrg != null);

            if (symTopPrg == myTopPrg)
            {
                //Same program ==> Apply the visibility mask
                System.Diagnostics.Debug.Assert(sym.Owner != null);
                if (sym.Owner == this || sym == this)
                {
                    //This My Own Symbol.
                    return true;
                }

                if (mask == 0 || sym.HasFlag(mask))
                {
                    if (sym.HasFlag(Flags.Global))
                    {
                        //The Symbol is a global Symbol => only SubNested PROGRAM can See it
                        if (this.Kind == Kinds.Function)
                        {
                            //I am a function, I cannot access anything else.
                            return false;
                        }

                        if (sym.Owner.Kind == Kinds.Function)
                        {
                            //Symbol declared inside a Function, cannot be accessed out of the function.                        
                            return false;
                        }

                        //Now the symbol must has been declared in an enclosing program of this one.
                        //In other words, one parent of this program must be the parent of the symbol.
                        return this.HasParent(sym.Owner);
                    }
                    else
                    {
                        //if mask == 0 ==> Local visibility ==> (symNearestKind == this)
                        // else ==> mask != Global && Mask == Public || Mask == Private;
                        System.Diagnostics.Debug.Assert((mask == 0) || (mask & (Flags.Public | Flags.Private)) != 0);
                        return sym.HasFlag(Flags.Public | Flags.Private);
                    }
                }
                else
                {
#if ALLOW_PRIV_PUBLIC_PROC_CALL_LOCAL_PROC
                    //Special case, only functions having the same parent can be called each other, this applied
                    // When a Private or Public Procedure call a Local Procedure with the same owner.
                    return this.Kind == Kinds.Function && sym.Kind == Kinds.Function && this.Owner == sym.Owner;
#else
                    return false;
#endif
                }
            }
            else
            {
                //Different programs ==> only public.
                return sym.HasFlag(Flags.Public);
            }
        }

        /// <summary>
        /// Determines if a Type is accessible from this Program.
        /// </summary>
        /// <param name="typeSym">The Type to be checked</param>
        /// <returns>true if the type is accessible, false otherwise</returns>
        public virtual bool IsTypeAccessible(TypedefSymbol typeSym)
        {
            return IsSymbolAccessible(typeSym, TypeVisibilityMask);
        }

        /// <summary>
        /// Determines if a Function is accessible from this Program.
        /// </summary>
        /// <param name="funSym">The Function to be checked</param>
        /// <returns>true if the type is accessible, false otherwise</returns>
        public virtual bool IsFunctionAccessible(FunctionSymbol funSym)
        {
            return IsSymbolAccessible(funSym, FunctionVisibilityMask);
        }

        /// <summary>
        /// Add a new VariableSymbol into this Scope.
        /// </summary>
        /// <param name="variable">The non-null VariableSymbol instance to add.</param>
        public void Add([NotNull] VariableSymbol variable)
        {
            System.Diagnostics.Debug.Assert(variable != null);
            _variables.Add(variable);
            //TODO SemanticDomain: store it in the root table.
        }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            DumpDomain(output, indentLevel, "Types", Types);
            DumpDomain(output, indentLevel, "WorkingStorageData", WorkingStorageData);
            DumpDomain(output, indentLevel, "LocalStorageData", LocalStorageData);
            DumpDomain(output, indentLevel, "LinkageData", LinkageData);
            DumpDomain(output, indentLevel, "Sections", Sections);
            DumpDomain(output, indentLevel, "Paragraphs", Paragraphs);
        }
    }
}

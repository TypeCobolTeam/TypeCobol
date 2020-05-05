using System.IO;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Represents a Program Symbol
    /// </summary>
    public class ProgramSymbol : ScopeSymbol
    {
        private readonly Container<VariableSymbol> _variables;

        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name"></param>
        public ProgramSymbol(string name)
            : base(name, Kinds.Program)
        {
            _variables = new Container<VariableSymbol>();
            Types = new Domain<TypedefSymbol>(this);
            FileData = new Domain<VariableSymbol>(this);
            GlobalStorageData = new Domain<VariableSymbol>(this);
            WorkingStorageData = new Domain<VariableSymbol>(this);
            LocalStorageData = new Domain<VariableSymbol>(this);
            LinkageData = new Domain<VariableSymbol>(this);
            Sections = new Domain<SectionSymbol>(this);
            Paragraphs = new Domain<ParagraphSymbol>(this);
            Functions = new Domain<FunctionSymbol>(this);
            Programs = new Domain<ProgramSymbol>(this);
        }

        /// <summary>
        /// All types defined in this program.
        /// </summary>
        public override Domain<TypedefSymbol> Types
        {
            get;
            protected set;
        }

        /// <summary>
        /// Data of the FILE SECTION of the program.
        /// </summary>
        public override Domain<VariableSymbol> FileData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Data of the GLOBAL-STORAGE SECTION of the program.
        /// </summary>
        public override Domain<VariableSymbol> GlobalStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Data of the WORKING-STORAGE SECTION of the program.
        /// </summary>
        public override Domain<VariableSymbol> WorkingStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Data of the LOCAL-STORAGE SECTION of the program.
        /// </summary>
        public override Domain<VariableSymbol> LocalStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Data of the LINKAGE SECTION of the program.
        /// </summary>
        public override Domain<VariableSymbol> LinkageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Sections of the program.
        /// </summary>
        public override Domain<SectionSymbol> Sections
        {
            get;
            protected set;
        }

        /// <summary>
        /// Paragraphs of the program.
        /// </summary>
        public override Domain<ParagraphSymbol> Paragraphs
        {
            get;
            protected set;
        }

        /// <summary>
        /// Functions/Procedures defined in the program.
        /// </summary>
        public override Domain<FunctionSymbol> Functions
        {
            get;
            protected set;
        }

        /// <summary>
        /// Nested Programs defined in the program.
        /// </summary>
        public override Domain<ProgramSymbol> Programs
        {
            get;
            protected set;
        }

        /// <summary>
        /// Add the variable to this Program.
        /// </summary>
        /// <param name="variable">VariableSymbol to add.</param>
        /// <returns>The given variable is returned.</returns>
        public void Add(VariableSymbol variable)
        {
            System.Diagnostics.Debug.Assert(variable != null);
            _variables.Add(variable);
            //TODO SemanticDomain: store it in the root table.
        }

        /// <summary>
        /// Is this program nested.
        /// </summary>
        public virtual bool IsNested => Owner != null && Kind == Kinds.Program && Owner.Kind == Kinds.Program;

        /// <summary>
        /// Get the Variable visibility mask.
        /// </summary>
        public virtual Flags VariableVisibilityMask => IsNested ? (Flags.Global | Flags.GLOBAL_STORAGE) : 0;

        /// <summary>
        /// Get the type visibility mask for a Program.
        /// </summary>
        public virtual Flags TypeVisibilityMask => IsNested ? (Flags.Global | Flags.Private | Flags.Public) : 0;

        /// <summary>
        /// Get the function visibility mask for a Program.
        /// </summary>
        public virtual Flags FunctionVisibilityMask => IsNested ? (Flags.Private | Flags.Public) : 0;

        /// <summary>
        /// Determines if a Symbol is accessible using and accessibility mask.
        /// </summary>
        /// <param name="sym">The symbol to be checked</param>
        /// <param name="mask">The accessibility mask</param>
        /// <returns>true if the symbol is accessible, false otherwise.</returns>
        private bool IsSymbolAccessible(Symbol sym, Flags mask)
        {
            System.Diagnostics.Debug.Assert(sym != null);
            if (sym.HasFlag(Flags.BuiltinSymbol))
                return true;//Builtin symbols are always accessible.
            Symbol symTopPrg = sym.TopParent(Kinds.Program);
            System.Diagnostics.Debug.Assert(symTopPrg != null);
            Symbol myTopPrg = TopParent(Kinds.Program);
            System.Diagnostics.Debug.Assert(myTopPrg != null);

            if (symTopPrg == myTopPrg)
            {//Same program ==> Apply the visibility mask
                System.Diagnostics.Debug.Assert(sym.Owner != null);
                if (sym.Owner == this || sym == this)
                {//This My Own Symbol.
                    return true;
                }
                if (mask == 0 || sym.HasFlag(mask))
                {
                    if (sym.HasFlag(Flags.Global))
                    {//The Symbol is a global Symbol => only SubNested PROGRAM can See it
                        if (this.Kind == Kinds.Function)
                        {//I am a function, I cannot access anything else.
                            return false;
                        }
                        if (sym.Owner.Kind == Kinds.Function)
                        {//Symbol declared inside a Function, cannot be accessed out of the function.                        
                            return false;
                        }
                        //Now the symbol must has been declared in an enclosing program of this one.
                        //In other words, one parent of this program must be the parent of the symbol.
                        return this.HasParent(sym.Owner);
                    }
                    else
                    {   //if mask == 0 ==> Local visibility ==> (symNearestKind == this)
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
            {//Different programs ==> only public.
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

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"IsNested: {IsNested}");
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        { 
            return v.VisitProgramSymbol(this, arg);
        }
    }
}

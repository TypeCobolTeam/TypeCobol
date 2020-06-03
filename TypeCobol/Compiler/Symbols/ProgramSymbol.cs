using System.IO;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
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
        public ProgramSymbol(string name) : base(name, Kinds.Program)
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
        /// Paragraps of the program.
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
            entry.Symbol.Owner = this;
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
                prgSym.Owner = null;
            }
        }

        internal override void DiscardSymbolsFromRoot()
        {
            var root = TopParent(Kinds.Root) as RootSymbolTable;
            System.Diagnostics.Debug.Assert(root != null);

            //Discard all types, variables, functions and nested programs from root
            foreach (var type in Types)
            {
                root.Discard(type);
            }
            foreach (var variable in _variables)
            {
                root.Discard(variable);
            }
            foreach (var function in Functions)
            {
                root.Discard(function);
            }
            foreach (var nestedProgram in Programs)
            {
                root.Discard(nestedProgram);
            }
        }

        /// <summary>
        /// Add the variable to this Program.
        /// </summary>
        /// <param name="variable">VariableSymbol to add.</param>
        /// <returns>The given variable is returned.</returns>
        public void Add(VariableSymbol variable)
        {
            System.Diagnostics.Debug.Assert(variable != null);

            //First store it in the root table.
            Symbol root = TopParent(Kinds.Root);
            ((RootSymbolTable)root)?.Store(variable);
            _variables.Add(variable);
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
        /// <param name="typeSym">The Function to be checked</param>
        /// <returns>true if the type is accessible, false otherwise</returns>
        public virtual bool IsFunctionAccessible(FunctionSymbol funSym)
        {
            return IsSymbolAccessible(funSym, FunctionVisibilityMask);
        }

        /// <summary>
        /// Get the top program.
        /// </summary>
        /// <param name="curPrg"></param>
        /// <returns></returns>
        public static ProgramSymbol GetTopProgram(ProgramSymbol curPrg)
        {
            ProgramSymbol top = (ProgramSymbol)curPrg.TopParent(Symbol.Kinds.Program);
            return top;
        }

        /// <summary>
        /// Resolve the given Symbol paths from this scope
        /// </summary>
        /// <param name="paths">The qualified path of the symbol reference in COBOL85 order</param>
        /// <param name="results">The All discovered candidate symbol accumulator</param>
        /// <param name="bRecurseEnglobingPrograms">true to recurse into englobing variables to look for global variable, false otherwise</param>
        /// <param name="visibilityMask">Visibility Mask</param>
        /// <returns>The referenced symbols if any</returns>
        private void ResolveReference(string[] paths, Container<VariableSymbol>.Entry results, bool bRecurseEnglobingPrograms, Symbol.Flags visibilityMask)
        {
            if (paths == null || paths.Length == 0 || paths[0] == null)
                return;

            string name = paths[0];
            if (this._variables.TryGetValue(name, out var candidates))
            {
                foreach (var candidate in candidates)
                {
                    if (visibilityMask != 0)
                    {
                        if ((candidate.Flag & visibilityMask) == 0)
                            continue;
                    }

                    if (candidate.IsMatchingPath(paths))
                        results.Add(candidate);
                }
            }

            if (bRecurseEnglobingPrograms)
            {
                if (results.Count == 0)
                {
                    ProgramSymbol curProg = this;
                    if (curProg.Owner != null && curProg.Owner.Kind == Kinds.Program)
                    {
                        curProg = (ProgramSymbol)curProg.Owner;
                        curProg.ResolveReference(paths, results, true, visibilityMask == 0 ? VariableVisibilityMask : visibilityMask);
                    }
                }
                else if ((visibilityMask & Flags.GLOBAL_STORAGE) == 0)
                {
                    // We have to search in TopProgram GLOBAL-STORAGE even if we already have found results locally.
                    var topPgm = GetTopProgram(this);
                    if (this != topPgm)
                    {
                        topPgm.ResolveReference(paths, results, false, Flags.GLOBAL_STORAGE);
                    }
                }
            }
        }

        /// <summary>
        /// Resolve the given SymbolReference from this scope
        /// </summary>
        /// <param name="symRef">The Symbol Reference to be resolved</param>
        /// <param name="bRecurseEnglobingPrograms">true to recurse into englobing variables to look for global variable, false otherwise</param>
        /// <returns>The referenced symbols if any</returns>
        public Container<VariableSymbol>.Entry ResolveReference(SymbolReference symRef, bool bRecurseEnglobingPrograms)
        {
            System.Diagnostics.Debug.Assert(symRef != null);
            return ResolveReference(SymbolReferenceToPath(symRef), bRecurseEnglobingPrograms);
        }

        /// <summary>
        /// Resolve the given Symbol paths from this scope
        /// </summary>
        /// <param name="paths">The qualified path of the symbol reference in COBOL85 order</param>
        /// <param name="bRecurseEnglobingPrograms">true to recurse into enclobing variables to look for global variable, false otherwise</param>
        /// <returns>The referenced symbols if any</returns>
        public Container<VariableSymbol>.Entry ResolveReference(string[] paths, bool bRecurseEnglobingPrograms)
        {
            if (paths == null || paths.Length == 0 || paths[0] == null)
                return null;

            var results = new Container<VariableSymbol>.Entry(paths[0]);
            ResolveReference(paths, results, bRecurseEnglobingPrograms, 0);
            return results;
        }

        /// <summary>
        /// Resolve a type.
        /// </summary>
        /// <param name="root">The Root Symbol Table</param>
        /// <param name="path">The type's path'</param>
        /// <returns>The Set of resolve types</returns>
        public override Container<TypedefSymbol>.Entry ResolveType(RootSymbolTable root, string[] path)
        {
            ProgramSymbol topPrg = (ProgramSymbol)TopParent(Kinds.Program);
            return ResolveSymbol<TypedefSymbol>(path, topPrg, root.LookupType);
        }

        /// <summary>
        /// Resolve a type.
        /// </summary>
        /// <param name="root">The Root Symbol Table</param>
        /// <param name="path">The type's path'</param>
        /// <returns>The Set of resolve types</returns>
        public override Container<ScopeSymbol>.Entry ResolveScope(RootSymbolTable root, string[] path)
        {
            ProgramSymbol topPrg = (ProgramSymbol)TopParent(Kinds.Program);
            return ResolveSymbol<ScopeSymbol>(path, topPrg, root.LookupScope);
        }

        /// <summary>
        /// Dump a section
        /// </summary>
        /// <param name="name">Section's name</param>
        /// <param name="section">The section to dump</param>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">indentation level</param>
        private void DumpSection(string name, Domain<VariableSymbol> section, TextWriter tw, int indentLevel)
        {
            if (section.Any())
            {
                string s = new string(' ', 2 * indentLevel);
                tw.Write(s);
                tw.Write(name);
                tw.WriteLine();
                foreach (var v in section)
                {
                    v.Dump(tw, indentLevel);
                }
                tw.WriteLine();
            }
        }

        public void DumpFileSection(TextWriter tw, int indentLevel)
        {
            DumpSection("FILE SECTION.", this.FileData, tw, indentLevel);
        }
        public void DumpGlobalSection(TextWriter tw, int indentLevel)
        {
            DumpSection("GLOBAL-STORAGE SECTION.", this.GlobalStorageData, tw, indentLevel);
        }
        public void DumpWorkingSection(TextWriter tw, int indentLevel)
        {
            DumpSection("WORKING-STORAGE SECTION.", this.WorkingStorageData, tw, indentLevel);
        }
        public void DumpLocalSection(TextWriter tw, int indentLevel)
        {
            DumpSection("LOCAL-STORAGE SECTION.", this.LocalStorageData, tw, indentLevel);
        }
        public void DumpLinkageSection(TextWriter tw, int indentLevel)
        {
            DumpSection("LINKAGE SECTION.", this.LinkageData, tw, indentLevel);
        }
        /// <summary>
        /// Dump the DataDivision
        /// </summary>
        /// <param name="tw"></param>
        /// <param name="indentLevel"></param>
        public void DumpDataDivision(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(s);
            tw.Write("DATA DIVISION.");
            tw.WriteLine();
            DumpFileSection(tw, indentLevel);
            DumpGlobalSection(tw, indentLevel);
            DumpWorkingSection(tw, indentLevel);
            DumpLocalSection(tw, indentLevel);
            DumpLinkageSection(tw, indentLevel);
        }

        /// <summary>
        /// Dump all nested Programs.
        /// </summary>
        /// <param name="tw"></param>
        /// <param name="indentLevel"></param>
        public void DumpNestedPrograms(TextWriter tw, int indentLevel)
        {
            foreach (var p in Programs)
            {
                p.Dump(tw, indentLevel);
                tw.WriteLine();
            }
        }

        /// <summary>
        /// Dump all nested procedure = Functions.
        /// </summary>
        /// <param name="tw"></param>
        /// <param name="indentLevel"></param>
        public void DumpFunctions(TextWriter tw, int indentLevel)
        {
            foreach (var f in this.Functions)
            {
                f.Dump(tw, indentLevel);
                tw.WriteLine();
            }
        }

        /// <summary>
        /// Dump this symbol in the given TextWriter instance.
        /// </summary>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">Indentation level</param>
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(s);
            tw.WriteLine("IDENTIFICATION DIVISION.");
            tw.Write(s);
            tw.Write("PROGRAM-ID. ");
            tw.Write(Name);
            tw.Write(".");
            tw.WriteLine();
            DumpDataDivision(tw, indentLevel);
            tw.WriteLine();
            tw.Write(s);
            tw.Write("PROCEDURE DIVISION.");
            this.Type?.Dump(tw, indentLevel + 1);
            tw.WriteLine();
            DumpFunctions(tw, indentLevel);
            DumpNestedPrograms(tw, indentLevel);
            tw.Write(s);
            tw.Write("END PROGRAM ");
            tw.Write(Name);
            tw.Write(".");
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitProgramSymbol(this, arg); }
    }
}

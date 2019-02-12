using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    public abstract class AbstractScope : Symbol, IScope
    {
        /// <summary>
        /// Empty constructor
        /// </summary>
        protected AbstractScope()
        {
        }

        /// <summary>
        /// Named constructor
        /// </summary>
        protected AbstractScope(String name, Kinds kind)
            : base(name, kind)
        {
        }

        public virtual Scope<TypedefSymbol> Types
        {
            get { return null; }
            protected set { ; }
        }

        public virtual Scope<VariableSymbol> FileData
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<VariableSymbol> GlobalStorageData
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<VariableSymbol> WorkingStorageData
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<VariableSymbol> LocalStorageData
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<VariableSymbol> LinkageStorageData
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<SectionSymbol> Sections
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<ParagraphSymbol> Paragraphs
        {
            get { return null; }
            protected set { }
        }

        public virtual Scope<FunctionSymbol> Functions
        {
            get { return null; }
            protected set {; }
        }

        public virtual Scope<ProgramSymbol> Programs
        {
            get { return null; }
            protected set { }
        }

        /// <summary>
        /// Lookup a program.
        /// </summary>
        /// <param name="rootScope">The top root scope</param>
        /// <param name="progName">The program name to be looked up</param>
        /// <param name="bCreate">true if an instance of the program symbol should be created, false otherwise.</param>
        /// <returns></returns>
        private Scope<ProgramSymbol>.Entry LookupProgram(AbstractScope rootScope, string progName, bool bCreate)
        {
            AbstractScope currentScope = null;
            AbstractScope stopScope = null;
            return LookupProgram(rootScope, progName, bCreate, out currentScope, out stopScope);
        }

        /// <summary>
        /// Lookup a program.
        /// </summary>
        /// <param name="rootScope">The top root scope</param>
        /// <param name="progName">The program name to be looked up</param>
        /// <param name="bCreate">true if an instance of the program symbol should be created, false otherwise.</param>
        /// <returns></returns>
        private Scope<ProgramSymbol>.Entry LookupProgram(AbstractScope rootScope, string progName, bool bCreate, out AbstractScope currentScope, out AbstractScope stopScope)
        {
            stopScope = rootScope;
            currentScope = this;
            Scope<ProgramSymbol>.Entry entry = null;
            while (currentScope != null)
            {
                var programs = currentScope.Programs;
                if (programs != null)
                {
                    entry = programs.Lookup(progName);
                    if (entry != null)
                    {
                        System.Diagnostics.Debug.Assert(entry.Count == 1);
                        currentScope = entry[0] as AbstractScope;
                        stopScope = currentScope;
                        break;
                    }
                }
                if (currentScope.Owner != null && currentScope != stopScope && currentScope.Owner.HasScope)
                {
                    currentScope = currentScope.Owner as AbstractScope;
                }
                else
                {
                    currentScope = null;
                }
            }
            if (entry == null && bCreate)
            {
                var programs = stopScope.Programs;
                if (programs != null)
                {
                    ProgramSymbol pgmSym = new ProgramSymbol(progName);
                    programs.Enter(pgmSym);
                    entry = programs.Lookup(progName);
                    currentScope = entry[0] as AbstractScope;
                    stopScope = currentScope;
                }
            }
            return entry;
        }

        /// <summary>
        /// Resolve a TypeDefinition from this current Scope.
        /// </summary>
        /// <param name="rootScope">The top rootScope</param>
        /// <param name="path">Looking path à la COBOL85 --> in Reverse order</param>
        /// <param name="bCreate">true if the TypeDef symbol shall be created if not existing, false otherwise.</param>
        /// <returns>The TypedefSymbol if found, null otherwise.</returns>
        public Scope<TypedefSymbol>.Entry ReverseResolveType(AbstractScope rootScope, string[] path, bool bCreate)
        {
            System.Diagnostics.Debug.Assert(rootScope != null);
            System.Diagnostics.Debug.Assert(path != null);
            System.Diagnostics.Debug.Assert(path.Length > 0);

            AbstractScope stopScope = rootScope;
            AbstractScope currentScope = this;
            for (int i = path.Length - 1; i >= 0 && currentScope != null; i--)
            {
                switch(i)
                {
                    case 0://We must look for a Type
                        {
                            AbstractScope startScope = currentScope;
                            while (currentScope != null)
                            {
                                var types = currentScope.Types;
                                if (types != null)
                                {
                                    Scope<TypedefSymbol>.Entry entry = types.Lookup(path[i]);
                                    if (entry != null)
                                    {
                                        return entry;
                                    }
                                }
                                if (currentScope.Owner != null && currentScope != stopScope && currentScope.Owner.HasScope)
                                {
                                    currentScope = currentScope.Owner as AbstractScope;
                                }
                                else
                                {
                                    currentScope = null;
                                }
                            }
                            if (startScope != null && bCreate)
                            {
                                var types = startScope.Types;
                                if (types != null)
                                {
                                    TypedefSymbol tdSym = new TypedefSymbol(path[i]);
                                    types.Enter(tdSym);
                                    Scope<TypedefSymbol>.Entry entry = types.Lookup(path[i]);
                                    return entry;
                                }
                            }
                        }
                        break;
                    case 1://We must look for a Program
                        {
                            Scope<ProgramSymbol>.Entry entry = LookupProgram(stopScope, path[i], bCreate, out currentScope, out stopScope);
                        //    Scope<ProgramSymbol>.Entry entry = null;
                        //    while (currentScope != null)
                        //    {
                        //        var programs = currentScope.Programs;
                        //        if (programs != null)
                        //        {
                        //            entry = programs.Lookup(path[i]);
                        //            if (entry != null)
                        //            {
                        //                System.Diagnostics.Debug.Assert(entry.Count == 1);
                        //                currentScope = entry[0] as AbstractScope;
                        //                stopScope = currentScope;
                        //                break;
                        //            }
                        //        }
                        //        if (currentScope.Owner != null && currentScope != stopScope &&
                        //            (currentScope.Owner.Kind == Kinds.Namespace || currentScope.Owner.Kind == Kinds.Program || currentScope.Owner.Kind == Kinds.Function))
                        //        {
                        //            currentScope = currentScope.Owner as AbstractScope;
                        //        }
                        //        else
                        //        {
                        //            currentScope = null;
                        //        }
                        //    }
                        //    if (entry == null && bCreate)
                        //    {
                        //        var programs = stopScope.Programs;
                        //        if (programs != null)
                        //        {
                        //            ProgramSymbol pgmSym = new ProgramSymbol(path[i]);
                        //            programs.Enter(pgmSym);
                        //            entry = programs.Lookup(path[i]);
                        //            currentScope = entry[0] as AbstractScope;
                        //            stopScope = currentScope;
                        //        }
                        //    }
                        }
                        break;
                    default://We are looking for a Namepace
                        //TODO
                        break;                        
                }
            }
            return null;
        }
    }
}

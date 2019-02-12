using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Reprsents a Program Symbol
    /// </summary>
    public class ProgramSymbol : AbstractScope
    {
        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name"></param>
        public ProgramSymbol(String name) : base(name, Kinds.Program)
        {
            Types = new Scope<TypedefSymbol>();
            FileData = new Scope<VariableSymbol>();
            GlobalStorageData = new Scope<VariableSymbol>();
            WorkingStorageData = new Scope<VariableSymbol>();
            LocalStorageData = new Scope<VariableSymbol>();
            LinkageStorageData = new Scope<VariableSymbol>();
            Sections = new Scope<SectionSymbol>();
            Paragraphs = new Scope<ParagraphSymbol>();
            Functions = new Scope<FunctionSymbol>();
            Programs = new Scope<ProgramSymbol>();
        }

        /// <summary>
        /// All types of this program.
        /// </summary>
        public override Scope<TypedefSymbol> Types
        {
            get;
            protected set;
        }

        /// <summary>
        /// File data scope of the program.
        /// </summary>
        public override Scope<VariableSymbol> FileData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Global Storage data scope of the program.
        /// </summary>
        public override Scope<VariableSymbol> GlobalStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Working Storage data scope of the program.
        /// </summary>
        public override Scope<VariableSymbol> WorkingStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Working Storage data scope of the program.
        /// </summary>
        public override Scope<VariableSymbol> LocalStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Linkage Storage data scope of the program.
        /// </summary>
        public override Scope<VariableSymbol> LinkageStorageData
        {
            get;
            protected set;
        }

        /// <summary>
        /// Section scope of the program.
        /// </summary>
        public override Scope<SectionSymbol> Sections
        {
            get;
            protected set;
        }

        /// <summary>
        /// Paragraph scope of the program.
        /// </summary>
        public override Scope<ParagraphSymbol> Paragraphs
        {
            get;
            protected set;
        }

        /// <summary>
        /// Functions scope of the program.
        /// </summary>
        public override Scope<FunctionSymbol> Functions
        {
            get;
            protected set;
        }

        /// <summary>
        /// Programs scope of the program.
        /// </summary>
        public override Scope<ProgramSymbol> Programs
        {
            get;
            protected set;
        }

        /// <summary>
        /// Enter a Program in this namespace
        /// </summary>
        /// <param name="name">Program's name</param>
        /// <returns>The ProgramSymbol</returns>
        public ProgramSymbol EnterProgram(String name)
        {
            Scope<ProgramSymbol>.Entry entry = Programs.Lookup(name);
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
                Programs.Remove(prgSym);
                prgSym.Owner = null;
            }
        }
    }
}

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
    }
}

using System.IO;
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

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            DumpDomain("Types", Types);
            DumpDomain("FileData", FileData);
            DumpDomain("GlobalStorageData", GlobalStorageData);
            DumpDomain("WorkingStorageData", WorkingStorageData);
            DumpDomain("LocalStorageData", LocalStorageData);
            DumpDomain("LinkageData", LinkageData);
            DumpDomain("Sections", Sections);
            DumpDomain("Paragraphs", Paragraphs);
            DumpDomain("Functions", Functions);
            DumpDomain("Programs", Programs);

            void DumpDomain<T>(string name, Domain<T> domain) where T : Symbol
            {
                if (domain != null && domain.Count > 0)
                {
                    string indent = new string(' ', 2 * indentLevel);
                    output.Write(indent);
                    output.WriteLine($"{name}:");
                    var level = indentLevel + 1;
                    foreach (var symbol in domain)
                    {
                        symbol.Dump(output, level);
                    }
                }
            }
        }
    }
}

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

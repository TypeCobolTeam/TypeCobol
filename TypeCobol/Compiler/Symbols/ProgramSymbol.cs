using System.IO;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Represents a Program Symbol
    /// </summary>
    public class ProgramSymbol : ScopeSymbol
    {
        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name">Name of the program.</param>
        public ProgramSymbol(string name)
            : base(name, Kinds.Program)
        {
            FileData = new Domain<VariableSymbol>(this);
            GlobalStorageData = new Domain<VariableSymbol>(this);
            Functions = new Domain<FunctionSymbol>(this);
            Programs = new Domain<ProgramSymbol>(this);
        }

        /// <summary>
        /// Data of the FILE SECTION of the program.
        /// </summary>
        public Domain<VariableSymbol> FileData { get; }

        /// <summary>
        /// Data of the GLOBAL-STORAGE SECTION of the program.
        /// </summary>
        public Domain<VariableSymbol> GlobalStorageData { get; }

        /// <summary>
        /// Functions/Procedures defined in the program.
        /// </summary>
        public Domain<FunctionSymbol> Functions { get; }

        /// <summary>
        /// Nested Programs defined in the program.
        /// </summary>
        public Domain<ProgramSymbol> Programs { get; }

        /// <summary>
        /// Is this program nested.
        /// </summary>
        public bool IsNested => Owner != null && Owner.Kind == Kinds.Program;

        /// <summary>
        /// Get the Variable visibility mask.
        /// </summary>
        protected override Flags VariableVisibilityMask => IsNested ? (Flags.Global | Flags.GLOBAL_STORAGE) : 0;

        /// <summary>
        /// Get the type visibility mask for a Program.
        /// </summary>
        protected override Flags TypeVisibilityMask => IsNested ? (Flags.Global | Flags.Private | Flags.Public) : 0;

        /// <summary>
        /// Get the function visibility mask for a Program.
        /// </summary>
        protected override Flags FunctionVisibilityMask => IsNested ? (Flags.Private | Flags.Public) : 0;

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            DumpDomain(output, indentLevel, "FileData", FileData);
            DumpDomain(output, indentLevel, "GlobalStorageData", GlobalStorageData);
            DumpDomain(output, indentLevel, "Functions", Functions);
            DumpDomain(output, indentLevel, "Programs", Programs);
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

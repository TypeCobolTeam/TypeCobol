using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// A Collection of symbol members
    /// </summary>
    public interface IScope
    {
        /// <summary>
        /// Type symbols
        /// </summary>
        Domain<TypedefSymbol> Types
        {
            get;
        }
        /// <summary>
        /// File data symbols
        /// </summary>
        Domain<VariableSymbol> FileData
        {
            get;
        }
        /// <summary>
        /// GlobalStorageData symbols
        /// </summary>
        Domain<VariableSymbol> GlobalStorageData
        {
            get;
        }
        /// <summary>
        /// WorkingStorageData symbols
        /// </summary>
        Domain<VariableSymbol> WorkingStorageData
        {
            get;
        }
        /// <summary>
        /// LocalStorageData symbols
        /// </summary>
        Domain<VariableSymbol> LocalStorageData
        {
            get;
        }
        /// <summary>
        /// LinkageData symbols
        /// </summary>
        Domain<VariableSymbol> LinkageData
        {
            get;
        }
        /// <summary>
        /// Sections symbols
        /// </summary>
        Domain<SectionSymbol> Sections
        {
            get;
        }
        /// <summary>
        /// Paragraphs symbols
        /// </summary>
        Domain<ParagraphSymbol> Paragraphs
        {
            get;
        }
        /// <summary>
        /// Functions symbols
        /// </summary>
        Domain<FunctionSymbol> Functions
        {
            get;
        }
        /// <summary>
        /// Functions symbols
        /// </summary>
        Domain<ProgramSymbol> Programs
        {
            get;
        }
    }
}

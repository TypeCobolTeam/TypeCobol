using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        Scope<TypedefSymbol> Types
        {
            get;
        }
        /// <summary>
        /// File data symbols
        /// </summary>
        Scope<VariableSymbol> FileData
        {
            get;
        }
        /// <summary>
        /// GlobalStorageData symbols
        /// </summary>
        Scope<VariableSymbol> GlobalStorageData
        {
            get;
        }
        /// <summary>
        /// WorkingStorageData symbols
        /// </summary>
        Scope<VariableSymbol> WorkingStorageData
        {
            get;
        }
        /// <summary>
        /// LocalStorageData symbols
        /// </summary>
        Scope<VariableSymbol> LocalStorageData
        {
            get;
        }
        /// <summary>
        /// LinkageStorageData symbols
        /// </summary>
        Scope<VariableSymbol> LinkageStorageData
        {
            get;
        }
        /// <summary>
        /// Sections symbols
        /// </summary>
        Scope<SectionSymbol> Sections
        {
            get;
        }
        /// <summary>
        /// Paragraphs symbols
        /// </summary>
        Scope<ParagraphSymbol> Paragraphs
        {
            get;
        }
        /// <summary>
        /// Functions symbols
        /// </summary>
        Scope<FunctionSymbol> Functions
        {
            get;
        }
        /// <summary>
        /// Functions symbols
        /// </summary>
        Scope<ProgramSymbol> Programs
        {
            get;
        }
    }
}

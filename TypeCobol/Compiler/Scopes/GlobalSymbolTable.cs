using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Scopes
{
    /// <summary>
    /// The Global Symbol Table is a special Namespace
    /// </summary>
    public class GlobalSymbolTable : NamespaceSymbol
    {
        /// <summary>
        /// The count of all variable created in this GlobalSymbolTable
        /// </summary>
        private uint _variableSymbolCounter = 0;
        /// <summary>
        /// Empty Constructor.
        /// </summary>
        public GlobalSymbolTable() : base("<<Global>>")
        {
            base.Kind = Kinds.Global;
        }

        /// <summary>
        /// The Count of Variable Symbol created
        /// </summary>
        public uint VariableSymbolCount => _variableSymbolCounter;

        /// <summary>
        /// Get the Next VariableSymbol Context.
        /// </summary>
        /// <returns></returns>
        public uint NextVariableSymbolIndex()
        {
            return _variableSymbolCounter++;
        }
    }
}

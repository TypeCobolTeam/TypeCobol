using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.CodeElements.Symbols
{
    /// <summary>
    /// Role of a symbol Token
    /// </summary>
    public enum SymbolRole
    {
        SymbolDefinition,
        SymbolReference,
        ExternalName
    }

    /// <summary>
    /// Information attached to a symbol Token
    /// </summary>
    public class SymbolInformation
    {
        /// <summary>
        /// Type of symbol
        /// </summary>
        public SymbolType Type { get; set; }

        /// <summary>
        /// Role of symbol Token
        /// </summary>
        public SymbolRole Role { get; set; }
    }
}

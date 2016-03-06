using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

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
        public SymbolInformation(SymbolRole role, SymbolType type)
        {
            Role = role;
            Type = type;
        }

        public SymbolInformation(SymbolRole role, SymbolType[] candidateSymbolTypes)
        {
            Role = role;
            CandidateSymbolTypes = candidateSymbolTypes;
        }

        /// <summary>
        /// Role of symbol Token
        /// </summary>
        public SymbolRole Role { get; set; }

        /// <summary>
        /// During the first parsing stage, the syntax alone does not always
        /// enable the parser to guess a unique symbol type for a given token.
        /// The parser inserts in this property a list of all possible symbol
        /// types for this token : only a lookup in the symbol tables at a
        /// later stage will resolve the ambiguity and give a final value
        /// to the Type property.
        /// </summary>
        public SymbolType[] CandidateSymbolTypes { get; set; }

        /// <summary>
        /// Type of the symbol token
        /// </summary>
        public SymbolType Type { get; set; }

        /// <summary>
        /// Several names can be qualified by the names of enclosing scopes, for example :
        ///    paragraphName IN sectionName
        ///    dataName IN dataName IN fileName
        ///    conditionName IN dataName IN dataName
        /// In this case, this property contains the ordered list of qualifying tokens.
        /// </summary>
        public Token[] QualifedBy { get; set; }
    }
}

using System;
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
        SymbolDefinitionOrReference, // class names in the repository paragraph can be either references or definitions
        ExternalName
    }

    /// <summary>
    /// Information attached to a symbol Token
    /// </summary>
    public class SymbolInformation
    {
        public SymbolInformation(Token symbolToken, SymbolRole role, SymbolType type)
        {
            SymbolToken = symbolToken;
            Role = role;
            Type = type;
        }

        public SymbolInformation(Token symbolToken, SymbolRole role, SymbolType[] candidateSymbolTypes)
        {
            SymbolToken = symbolToken;
            Role = role;
            CandidateSymbolTypes = candidateSymbolTypes;
        }

        /// <summary>
        /// Symbol token
        /// </summary>
        public Token SymbolToken { get; set; }

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
        ///    dataName1 IN dataName2 IN fileName
        ///    conditionName IN dataName3 IN dataName4
        /// In this case, this property contains the ordered list of qualifying tokens, for exemple :
        ///    paragraphName => QualifiedBy = { sectionName }
        ///    dataName1 => QualifiedBy = { dataName2, fileName }
        ///    conditionName => QualifiedBy = { dataName3, dataName4 }
        /// </summary>
        public Token[] QualifedBy { get; set; }

        /// <summary>
        /// Several names can be qualified by the names of enclosing scopes, for example :
        ///    paragraphName IN sectionName
        ///    dataName1 IN dataName2 IN fileName
        ///    conditionName IN dataName3 IN dataName4
        /// In this case, this property contains the ordered list of qualifying tokens, for exemple :
        ///    sectionName => QualifierFor = paragraphName
        ///    fileName => QualifierFor = dataName2
        ///    dataName2 => QualifierFor = dataName1
        /// </summary>
        public Token QualifierFor { get; set; }
    }
}

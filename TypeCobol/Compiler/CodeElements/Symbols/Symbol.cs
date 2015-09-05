using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Symbols defined or referenced in the Cobol syntax
    /// </summary>
    public class Symbol
    {
        /// <summary>
        /// Attach a symbol type to a source token in a declaration statement 
        /// </summary>
        /// <param name="nameToken">Token representing the name of the symbol (TokenFamily.Symbol or TokenFamily.AlphanumericLiteral)</param>
        /// <param name="type">Type of symbol</param>
        public Symbol(Token nameToken, SymbolType type)
        {
            NameToken = nameToken;
            Type = type;
        }

        /// <summary>
        /// Name of the symbol
        /// </summary>
        public string Name
        {
            get
            {
                if(NameToken.TokenFamily == TokenFamily.Symbol || 
                        NameToken.TokenFamily == TokenFamily.SpecialRegisterKeyword )
                {
                    return NameToken.Text;
                }
                else if(NameToken.TokenFamily == TokenFamily.AlphanumericLiteral)
                {
                    return ((AlphanumericLiteralValue)NameToken.LiteralValue).Text;
                }
                else if(NameToken.TokenType == TokenType.SymbolicCharacter)
                {
                    return NameToken.Text;
                }
                else
                {
                    throw new InvalidOperationException("A symbol token can not be of type : " + NameToken.TokenType);
                }
            }
        }

        /// <summary>
        /// Token defining the name of the symbol in source text
        /// </summary>
        public Token NameToken { get; private set; }

        /// <summary>
        /// Type of the symbol
        /// </summary>
        public SymbolType Type { get; set; }

        // -- Override Equals & GetHashCode --

        public override bool Equals(object obj)
        {
            Symbol otherSymbol = obj as Symbol;
            if(otherSymbol == null)
            {
                return false;
            }
            else
            {
                return Type == otherSymbol.Type && 
                    Name.Equals(otherSymbol.Name, StringComparison.OrdinalIgnoreCase);
            }
        }

        public override int GetHashCode()
        {
 	        return Type.GetHashCode() * 11 + Name.GetHashCode();
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return Name;
        }
    }
}

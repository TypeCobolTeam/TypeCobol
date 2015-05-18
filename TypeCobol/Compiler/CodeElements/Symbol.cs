using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Symbols defined or referenced in the Cobol syntax
    /// </summary>
    public class Symbol
    {
        public Symbol(string name, SymbolType type)
        {
            Name = name;
            Type = type;
        }

        public string Name { get; set; }

        public SymbolType Type { get; set; }

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
                    Name.Equals(otherSymbol.Name);
            }
        }

        public override int GetHashCode()
        {
 	        return Name.GetHashCode();
        }
    }
}

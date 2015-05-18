using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// symbolic-character-1 is a user-defined word and must contain at least one
    /// alphabetic character. The same symbolic-character can appear only once in
    /// a SYMBOLIC CHARACTERS clause.
    /// The internal representation of symbolic-character-1 is the internal
    /// representation of the character that is represented in the specified character
    /// set. 
    /// </summary>
    public class SymbolicCharacter : Symbol
    {
        public SymbolicCharacter(string symbolicCharacter) :
            base(symbolicCharacter, SymbolType.SymbolicCharacter)
        { }
    }
}

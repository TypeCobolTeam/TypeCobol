using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// alphabet-name-1 specifies a collating sequence when used in:
    /// - The PROGRAM COLLATING SEQUENCE clause of the object-computer
    /// paragraph
    /// - The COLLATING SEQUENCE phrase of the SORT or MERGE statement
    /// alphabet-name-1 specifies a character code set when used in:
    /// - The FD entry CODE-SET clause
    /// - The SYMBOLIC CHARACTERS clause
    /// </summary>
    public class AlphabetName : Symbol
    {
        public AlphabetName(string userDefinedWord) :
            base(userDefinedWord, SymbolType.AlphabetName)
        { }
    }
}

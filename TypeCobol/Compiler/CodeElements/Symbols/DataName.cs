using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Declared int the DATA DIVISION.
    /// Identifies a data item used in the program.
    /// </summary>
    public class DataName : Symbol
    {
        public DataName(string userDefinedWord) :
            base(userDefinedWord, SymbolType.DataName)
        { }
    }
}

using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Declared in the DATA DIVISION.
    /// Identifies a data item used in the program.
    /// </summary>
    public class DataName : Symbol
    {
        public DataName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.DataName)
        { }
    }
}

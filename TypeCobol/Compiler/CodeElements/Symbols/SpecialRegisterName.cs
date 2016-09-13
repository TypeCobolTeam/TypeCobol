using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Identifies a special register.
    /// </summary>
    public class SpecialRegisterName : Symbol
    {
        public SpecialRegisterName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.SpecialRegisterName)
        { }
    }
}

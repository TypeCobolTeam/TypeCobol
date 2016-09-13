using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// mnemonic-name-2 follows the rules of formation for user-defined names. 
    /// mnemonic-name-2 can be referenced only in the SET statement. 
    /// mnemonic-name-2 can qualify condition-1 or condition-2 names.
    /// </summary>
    public class MnemonicForUPSISwitchName : Symbol
    {
        public MnemonicForUPSISwitchName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.MnemonicForUPSISwitchName)
        { }
    }
}

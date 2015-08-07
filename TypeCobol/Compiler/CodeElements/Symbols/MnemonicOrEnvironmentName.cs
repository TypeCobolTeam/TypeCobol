using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// mnemonic-name-1 follows the rules of formation for user-defined names. 
    /// mnemonic-name-1 can be used in ACCEPT, DISPLAY, and WRITE statements. 
    /// Mnemonic-names and environment-names need not be unique. If you
    /// choose a mnemonic-name that is also an environment-name, its definition
    /// as a mnemonic-name will take precedence over its definition as an
    /// environment-name.
    /// </summary>
    public class MnemonicOrEnvironmentName : Symbol
    {

        /// <summary>
        /// As a MnemonicForEnvironmentName can use the same name as an EnvironmentName, 
        /// this constructor allow to create a MnemonicOrEnvironmentName whose type will be know only
        /// after the parsing phase.
        /// 
        /// The SymbolType is unknown and will be changed later.
        /// </summary>
        /// <param name="userDefinedWord"></param>
        public MnemonicOrEnvironmentName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.Unknown)
        { }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="userDefinedWord"></param>
        /// <param name="symbolType"></param>
        protected MnemonicOrEnvironmentName(Token userDefinedWord, SymbolType symbolType) :
            base(userDefinedWord, symbolType)
        { }
    }
}

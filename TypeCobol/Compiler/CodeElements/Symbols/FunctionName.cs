using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Identifies an intrinsic function.
    /// </summary>
    public class FunctionName : Symbol
    {
        public FunctionName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.FunctionName)
        { }
    }
}

using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// An alphanumeric literal or national literal that contains the name of the
    /// method. The name must conform to the rules of formation for a Java
    /// method name. Method names are used directly, without translation. The
    /// method name is processed in a case-sensitive manner.
    /// </summary>
    public class MethodName : Symbol
    {
        public MethodName(Token alphanumOrNationalLiteral) :
            base(alphanumOrNationalLiteral, SymbolType.MethodName)
        { }
    }
}

using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// CLASS class-name-1 IS
    /// Provides a means for relating a name to the specified set of characters
    /// listed in that clause. class-name-1 can be referenced only in a class
    /// condition. The characters specified by the values of the literals in this
    /// clause define the exclusive set of characters of which this class consists.
    /// </summary>
    public class CharsetClassName : Symbol
    {
        public CharsetClassName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.CharsetClassName)
        { }
    }
}

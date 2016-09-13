using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// An index-name identifies an index. An index can be regarded as a private special
    /// register that the compiler generates for working with a table. You name an index
    /// by specifying the INDEXED BY phrase in the OCCURS clause that defines a table.
    /// An index-name is not the same as the name of an index data item, and an
    /// index-name cannot be used like a data-name.
    /// </summary>
    public class IndexName : Symbol
    {
        public IndexName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.IndexName)
        { }
    }
}

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeModel
{
    interface SymbolUser
    {
        IList<QualifiedName> Symbols { get; }
    }
}

using TypeCobol.Compiler.CodeElements.Expressions;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    public class AddStatement : CodeElement
    {
        public AddStatement() : base(CodeElementType.AddStatement)
        {
            affectations = new Dictionary<SymbolReference<DataName>, Expression>();
        }
        public Dictionary<SymbolReference<DataName>, Expression> affectations { get; set; }
    }
}
